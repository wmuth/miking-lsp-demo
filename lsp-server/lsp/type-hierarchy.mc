include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"
include "./progress.mc"

let emptyRpcResponse = lam id. jsonKeyObject [
  ("jsonrpc", JsonString "2.0"),
  ("id", JsonInt id),
  ("result", JsonNull ())
]

let generateLocationLinks__LSPTypeHierarchy: Info -> Option JsonValue =
  lam info.
    match info with Info r then
      Some (jsonKeyObject [
        ("uri", JsonString r.filename),
        ("range", infoToRangeUnwrap r)
      ])
    else 
      None ()

lang LSPTypeHierarchy = LSPRoot + LSPProgress
  syn Message =
  | TypeHierarchyPrepare {
    id: Int,
    textDocument: TextDocumentPositionParams
  }
  | TypeHierarchySupertypes {
    id: Int,
    symbol: Int
  }
  | TypeHierarchySubtypes {
    id: Int,
    symbol: Int
  }

  sem getMessage request =
  | "textDocument/prepareTypeHierarchy" ->
    match request.id with Some id in
    TypeHierarchyPrepare {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }
  | "typeHierarchy/supertypes" ->
    match request.id with Some id in
    match mapLookup "item" request.params with Some (JsonObject item) in
    match mapLookup "data" item with Some JsonInt symbol in
    TypeHierarchySupertypes {
      id = id,
      symbol = symbol
    } 
  | "typeHierarchy/subtypes" ->
    match request.id with Some id in
    match mapLookup "item" request.params with Some (JsonObject item) in
    match mapLookup "data" item with Some JsonInt symbol in
    TypeHierarchySubtypes {
      id = id,
      symbol = symbol
    } 

  sem createTypeHierachyItem : LSPExecutionContext -> Name -> (Map Int Name, JsonValue)
  sem createTypeHierachyItem context =| typName ->
    let files = mapValues context.environment.files in
    let types = foldl (
      lam acc. lam file.
        mapUnion acc file.types
    ) (mapEmpty nameSymCmp) files in
    let lookup = (flip mapLookup) types in

    let typ = lookup typName in

    let res = optionMap (
      lam typ. match typ with { location=Info r } in
      let symbol = sym2hash (gensym ()) in

      let res = jsonKeyObject [
        ("name", JsonString (nameGetStr typName)),
        ("kind", JsonInt (getSymbolKind (TypeParameter ()))),
        ("uri", JsonString r.filename),
        ("range", infoToRangeUnwrap r),
        ("selectionRange", infoToRangeUnwrap r),
        ("data", JsonInt symbol) -- keep track of the symbol to subsequent type hierarchy calls
      ] in

      (res, symbol)
    ) typ in

    match res with Some (res, symbol) then
      (
        mapSingleton subi symbol typName,
        res
      )
    else
      (
        mapEmpty subi,
        JsonNull ()
      )

  sem execute context =
  | TypeHierarchyPrepare { id = id, textDocument = {
    uri = uri,
    line = line,
    character = character
  } } -> 
    -- Add 1 to incoming line and character to match the 1-based indexing of LSP
    let line = addi line 1 in
    let uri = stripUriProtocol uri in

    match mapLookup uri context.environment.files with Some file then
      let progress = createProgress context.sendNotification in
      progress.reportMsg 0.0 "Generating usages";

      let typesResult = findInfo file.typeLocations uri line character in

      eprintln (join ["Looking up type hierarchy for ", uri, " at ", int2string line, ":", int2string character]);
      eprintln (join ["Found usage result: ", (compose bool2string optionIsSome) typesResult]);

      let res = optionMap (lam typRes.
        match typRes with (Info r, typNames) in
        let lookup = (flip mapLookup) file.types in
        let types = filterMap lookup typNames in

        map (
          lam v. match v with (typ, typName) in
            let symbol = sym2hash (gensym ()) in

            let res = jsonKeyObject [
              ("name", JsonString (nameGetStr typName)),
              ("kind", JsonInt (getSymbolKind (TypeParameter ()))),
              ("uri", JsonString r.filename),
              ("range", infoToRangeUnwrap r),
              ("selectionRange", infoToRangeUnwrap r),
              ("data", JsonInt symbol) -- keep track of the symbol to subsequent type hierarchy calls
            ] in

            (res, symbol, typName)
        ) (zip types typNames)
      ) typesResult in

      progress.finish (None ());

      match res with Some res then
        let typeSymbols = mapFromSeq subi (map (
          lam v.
            match v with (_, symbol, typName) in (symbol, typName)
        ) res) in

        let response = jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt id),
          ("result", JsonArray (map (lam res. res.0) res))
        ] in

        {
          response = Some response,
          environment = {
            context.environment with
            typeSymbols = mapUnion context.environment.typeSymbols typeSymbols
          }
        }
      else 
        { response = Some (emptyRpcResponse id), environment = context.environment }
    else
      { response = Some (emptyRpcResponse id), environment = context.environment }
  | TypeHierarchySupertypes { id = id, symbol = symbol } ->
    let files = mapValues context.environment.files in
    let types = foldl (
      lam acc. lam file.
        mapUnion acc file.types
    ) (mapEmpty nameSymCmp) files in
    let lookup = (flip mapLookup) types in

    let typName = mapLookup symbol context.environment.typeSymbols in
    let typ = optionBind typName lookup in
    let supers = optionMap (lam typ. typ.super) typ in
    let responses = optionMap (map (createTypeHierachyItem context)) supers in

    match responses with Some responses then
      let symbolsCache = foldl (lam acc. lam response. match response with (symbolsCache, response) in mapUnion acc symbolsCache) (mapEmpty subi) responses in
      let responses = map (lam response. match response with (symbolsCache, response) in response) responses in

      let response = jsonKeyObject [
        ("jsonrpc", JsonString "2.0"),
        ("id", JsonInt id),
        ("result", JsonArray responses)
      ] in

      {
        response = Some response,
        environment = {
          context.environment with
          typeSymbols = mapUnion context.environment.typeSymbols symbolsCache
        }
      }
    else
      {
        response = Some (emptyRpcResponse id),
        environment = context.environment
      }
  | TypeHierarchySubtypes { id = id, symbol = symbol } ->
      -- TODO: Implement
      {
        response = Some (emptyRpcResponse id),
        environment = context.environment
      }
end