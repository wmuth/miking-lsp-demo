include "json.mc"

include "../../lib/utils.mc"

include "./symbol-kind.mc"
include "./utils.mc"
include "./root.mc"

let generateLocationLinks__LSPWorkspaceSymbol: Info -> Option JsonValue =
  lam info.
    match info with Info r then
      Some (jsonKeyObject [
        ("uri", JsonString r.filename),
        ("range", infoToRangeUnwrap r)
      ])
    else 
      None ()

recursive let take = lam n. lam xs.
  if eqi n 0 then [] else
  match xs with [x] ++ xs then
    concat [x] (take (subi n 1) xs)
  else
    []
end

lang LSPWorkspaceSymbol = LSPRoot + LSPSymbolKind
  syn Message =
  | WorkspaceSymbol {
    id: Int,
    query: String
  }

  sem getMessage request =
  | "workspace/symbol" ->
    match request.id with Some id in
    match mapLookup "query" request.params with Some JsonString query in
    WorkspaceSymbol {
      id = id,
      query = query
    }

  -- sem findDefinitions: Map Name [Info] -> Name -> Option DefinitionInformation
  -- sem findDefinitions definitions =| name ->
  --   let definitions = mapLookup name definitions in
  --   optionMap (lam locations. {
  --     name = name,
  --     locations = locations
  --   }) definitions

  sem execute context =
  | WorkspaceSymbol { id = id, query = query } -> 
    let query = str2lower query in

    let files = mapValues context.environment.files in

    let definitions = foldl (
      lam acc. lam file.
        mapUnionWith concat acc file.definitions
    ) (mapEmpty nameCmp) files in

    let definitions = mapMap (
      filter (lam definition. definition.exported)
    ) definitions in

    let definitions = mapFilter (
      lam definitions.
        geqi (length definitions) 1
    ) definitions in

    let definitions = mapToSeq definitions in

    let definitions = filter (
      lam definition.
        match definition with (name, _) in
        let name = (compose str2lower nameGetStr) name in
        strStartsWith query name
    ) definitions in

    let definitions = take 10 definitions in

    -- let definitionResult = findDefinitionLinearly uri definitions line character in
    -- let usageResults = optionMap (findUsagesLinearly usages) definitionResult in
    -- let response = generateLocationLinks2 id usageResults in

    -- {
    --   response = Some response,
    --   environment = context.environment
    -- }

    let f = lam definition.
      match definition with (name, definitions) in
      filterMap (
        lam definition.
          match generateLocationLinks__LSPWorkspaceSymbol definition.location with Some location then
            Some (jsonKeyObject [
              ("name", JsonString (nameGetStr name)),
              ("kind", JsonInt (getSymbolKind definition.kind)),
              ("location", location)
            ])
          else 
            None ()
      ) definitions
    in

    let response = jsonKeyObject [
      ("jsonrpc", JsonString "2.0"),
      ("id", JsonInt id),
      ("result", JsonArray (flatMap f definitions))
    ] in

    {
      response = Some response,
      environment = context.environment
    }
    
end