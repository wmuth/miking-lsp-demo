include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"
include "./progress.mc"

lang LSPGotoDefinition = LSPRoot + LSPProgress
  syn Message =
  | GotoDefinition {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

  sem getMessage request =
  | "textDocument/definition" ->
    match request.id with Some id in
    GotoDefinition {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

  type UsageInformation = {
    location: Info,
    names: [Name]
  }

  type DefinitionInformation = {
    from: Info,
    to: Info
  }

  sem generateLocationLinks: Int -> [DefinitionInformation] -> JsonValue
  sem generateLocationLinks id =
  | locations ->
    -- TODO: if definition and variable overlap,
    -- truncate the definition position to end
    -- at the start of the variable position.
    -- Otherwise, LSP will return no result.
    
    let result = if geqi (length locations) 1 then
      let locations = filterMap (
        lam location.
          match location with {from = Info from, to = Info to} then
            Some (jsonKeyObject [
              ("originSelectionRange", infoToRangeUnwrap from),
              ("targetUri", JsonString to.filename),
              ("targetRange", infoToRangeUnwrap to),
              ("targetSelectionRange", infoToRangeUnwrap to)
            ])
          else
            None ()
      ) locations in

      JsonArray locations
    else
      JsonNull ()
    in

    jsonKeyObject [
      ("jsonrpc", JsonString "2.0"),
      ("id", JsonInt id),
      ("result", result)
    ]

  sem findUsageLinearly: URI -> Map Info [Name] -> Int -> Int -> Option UsageInformation
  sem findUsageLinearly uri usages line =| character ->
    optionMap (
      lam res.
        match res with (info, value) in
        { location = info, names = value }
    ) (findInfo usages uri line character)

  sem findDefinitions: Map Name [(Info, SymbolKind)] -> UsageInformation -> [DefinitionInformation]
  sem findDefinitions definitions =| usage ->
    let locations = filterMap (lam name. mapLookup name definitions) usage.names in
    flatMap (map (lam location. { from = usage.location, to = location.0 })) locations

  sem findGotosLinearly: Map Info [Info] -> URI -> Int -> Int -> [DefinitionInformation]
  sem findGotosLinearly gotos uri line =| character ->
    let res = optionMap (
      lam res.
        match res with (info, locations) in
        map (lam location. { from = info, to = location }) locations
    ) (findInfo gotos uri line character) in

    optionGetOr [] res

  sem execute context =
  | GotoDefinition { id = id, textDocument = {
    uri = uri,
    line = line,
    character = character
  } } -> 
    -- Add 1 to incoming line and character to match the 1-based indexing of LSP
    let line = addi line 1 in
    let uri = stripUriProtocol uri in

    match mapLookup uri context.environment.files with Some file then
      let files = mapValues context.environment.files in
      let environment = mapLookup uri context.environment.files in

      let progress = createProgress context.sendNotification in
      progress.reportMsg 0.0 "Generating usages";

      let usages = file.usages in

      progress.reportMsg 0.2 "Generating definitions";

      let definitions = foldl (
        lam acc. lam file.
          mapUnionWith concat acc file.definitions
      ) (mapEmpty nameSymCmp) files in

      progress.reportMsg 0.6 "Finding usages";

      let usageResult = findUsageLinearly uri usages line character in

      progress.reportMsg 0.8 "Finding definitions";
      let definitions = optionMap (findDefinitions definitions) usageResult in

      let locations = (compose join filterOption) [definitions] in

      let response = generateLocationLinks id locations in

      progress.finish (None ());

      {
        response = Some response,
        environment = context.environment
      }
    else
      {
        response = None (),
        environment = context.environment
      }
    
end