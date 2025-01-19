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

  sem generateLocationLinks: Int -> [Info] -> JsonValue
  sem generateLocationLinks id =
  | locations ->
    -- TODO: if definition and variable overlap,
    -- truncate the definition position to end
    -- at the start of the variable position.
    -- Otherwise, LSP will return no result.
    
    let result = if geqi (length locations) 1 then
      let locations = filterMap (
        lam definition.
          match definition with Info r then
            Some (jsonKeyObject [
              ("uri", JsonString r.filename),
              infoToRangeUnwrap r
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

  sem findDefinitions: Map Name [Info] -> Name -> Option DefinitionInformation
  sem findDefinitions definitions =| name ->
    let locations = mapLookup name definitions in
    -- eprintln (join [
    --   "All "
    -- ])
    eprintln (join [
      "Locations for ",
      nameGetStr name,
      ": ",
      strJoin ", " (optionGetOr [] (optionMap (map info2str) locations))
    ]);
    optionMap (lam locations. {
      name = name,
      locations = locations
    }) locations

  sem findGotosLinearly: Map Info [Info] -> URI -> Int -> Int -> Option [Info]
  sem findGotosLinearly gotos uri line =| character ->
  optionMap (
    lam res.
      match res with (info, location) in
      location
  ) (findInfo gotos uri line character)

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
    let environment = mapLookup uri context.environment.files in

    let usages = foldl (
      lam acc. lam file.
        mapUnionWith concat acc file.usages
    ) (mapEmpty infoCmp) files in

    let definitions = foldl (
      lam acc. lam file.
        mapUnionWith concat acc file.definitions
    ) (mapEmpty nameSymCmp) files in
    
    let gotos = optionBind environment (
      lam environment.
        findGotosLinearly environment.gotos uri line character
    ) in

    let usageResult = findUsageLinearly uri usages line character in
    let names = optionMap (lam usageResult. usageResult.names) usageResult in
    let definitions = optionMap (filterMap (findDefinitions definitions)) names in
    let definitions = optionMap (flatMap (lam definition. definition.locations)) definitions in

    let locations = flatten (filterOption [
      definitions,
      gotos
    ]) in

    let response = generateLocationLinks id locations in

    {
      response = Some response,
      environment = context.environment
    }
    
end