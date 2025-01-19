include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"

lang LSPGotoDefinition = LSPRoot
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
    name: Name,
    locations: [Info]
  }

  sem generateLocationLinks: Int -> Option [DefinitionInformation] -> JsonValue
  sem generateLocationLinks id =
  | definitions ->
    -- TODO: if definition and variable overlap,
    -- truncate the definition position to end
    -- at the start of the variable position.
    -- Otherwise, LSP will return no result.
    
    let result = match definitions with Some definitions then
      let definitions = flatMap (lam definition. definition.locations) definitions in
      let locations = filterMap (
        lam definition.
          match definition with Info r then
            Some (jsonKeyObject [
              ("uri", JsonString r.filename),
              infoToRangeUnwrap r
            ])
          else
            None ()
      ) definitions in

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
    let usages = mapToSeq usages in

    let foundUsages = filterMap (
      lam usage.
        match usage with (info, names) in
        if infoCollision info uri line character
          then Some { location = info, names = names }
          else None ()
    ) usages in

    match foundUsages with [first] ++ rest then
      let f = lam v1. lam v2.
        if infoContainsInfo v1.location v2.location then v1 else v2
      in

      Some (foldl f first rest)
    else
      None()

  sem findDefinitions: Map Name [Info] -> Name -> Option DefinitionInformation
  sem findDefinitions definitions =| name ->
    let definitions = mapLookup name definitions in
    optionMap (lam locations. {
      name = name,
      locations = locations
    }) definitions

  sem execute context =
  | GotoDefinition { id = id, textDocument = {
    uri = uri,
    line = line,
    character = character
  } } -> 
    -- Add 1 to incoming line and character to match the 1-based indexing of LSP
    let line = addi line 1 in
    let uri = stripUriProtocol uri in

    let files = mapValues context.environment.files in

    let usages = foldl (
      lam acc. lam file.
        mapUnionWith concat acc file.usages
    ) (mapEmpty infoCmp) files in

    let definitions = foldl (
      lam acc. lam file.
        mapUnionWith concat acc file.definitions
    ) (mapEmpty nameSymCmp) files in
    

    let usageResult = findUsageLinearly uri usages line character in
    let names = optionMap (lam usageResult. usageResult.names) usageResult in
    let definitions = optionMap (filterMap (findDefinitions definitions)) names in
    let response = generateLocationLinks id definitions in

    {
      response = Some response,
      environment = context.environment
    }
    
end