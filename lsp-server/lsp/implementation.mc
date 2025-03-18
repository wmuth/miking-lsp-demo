include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"

-- let findDefinitionLinearly: URI -> Map Name [(Info, use LSPSymbolKind in SymbolKind)] -> Int -> Int -> Option Name =
let findDefinitionLinearly: URI -> Map Name [use LanguageServerRoot in LSPDefinition] -> Int -> Int -> Option Name =
  lam uri. lam definitions. lam line. lam character.
    -- Make Into [Name, [Info]] pairs
    let definitions = mapToSeq definitions in
    -- Make into [Name, Info] pairs
    let definitions = flatMap (lam definition. match definition with (name, infos) in map (lam info. (name, info.location)) infos) definitions in

    let foundDefinitions = filterMap (
      lam definition.
        match definition with (name, Some info) then
          if infoCollision info uri line character
            then Some { location = info, name = name }
            else None ()
        else None ()
    ) definitions in

    match foundDefinitions with [first] ++ rest then
      let f = lam v1. lam v2.
        if infoContainsInfo v1.location v2.location then v1 else v2
      in

      Some ((foldl f first rest).name)
    else
      None()

let generateLocationLinks2: Int -> Option [Info] -> JsonValue =
  lam id. lam definitions.
    let result = match definitions with Some definitions then
      let locations = filterMap (
        lam definition.
          match definition with Info r then
            Some (jsonKeyObject [
              ("uri", JsonString r.filename),
              ("range", infoToRangeUnwrap r)
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

let findUsagesLinearly: Map Info [Name] -> Name -> [Info] =
  lam usages. lam name.
    -- Make Into [Info, [Name]] pairs
    let usages = mapToSeq usages in
    -- Make into [Info, Name] pairs
    let usages = flatMap (lam definition. match definition with (info, names) in map (lam name. (info, name)) names) usages in

    filterMap (
      lam usage.
        match usage with (info, usageName) in
        if nameEq usageName name 
          then Some info
          else None ()
    ) usages

lang LSPImplementation = LSPRoot
  syn Message =
  | Implementation {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

  sem getMessage request =
  | "textDocument/implementation" ->
    match request.id with Some id in
    Implementation {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

  -- sem findDefinitions: Map Name [Info] -> Name -> Option DefinitionInformation
  -- sem findDefinitions definitions =| name ->
  --   let definitions = mapLookup name definitions in
  --   optionMap (lam locations. {
  --     name = name,
  --     locations = locations
  --   }) definitions

  sem execute context =
  | Implementation { id = id, textDocument = {
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

    let definitionResult = findDefinitionLinearly uri definitions line character in
    let usageResults = optionMap (findUsagesLinearly usages) definitionResult in
    let response = generateLocationLinks2 id usageResults in

    {
      response = Some response,
      environment = context.environment
    }

    -- TODO: if definition and variable overlap,
    -- truncate the definition position to end
    -- at the start of the variable position.
    -- Otherwise, LSP will return no result.
    
end