include "json.mc"
include "mexpr/type-check.mc"
include "mexpr/mexpr.mc"
include "mexpr/keyword-maker.mc"

include "../../lib/utils.mc"
include "./utils.mc"
include "./root.mc"

-- Print debug information
let __debug = false

let debugPrintDefinitionLookup = lam definitionLookup.
    let seq = mapToSeq definitionLookup in
    let f = lam x. eprintln (join ["\t", nameGetStr x.0, ": ", info2str x.1]); () in
    eprintln (join ["Definition Lookup (", int2string (length seq), "):"]);
    iter f seq; ()

let debugPrintVariableLookup = lam variableLookup.
    let seq = mapToSeq variableLookup in
    let f = lam x. eprintln (join ["\t", nameGetStr (x.1).0, ": ", info2str x.0]); () in
    eprintln (join ["Variable Lookup (", int2string (length seq), "):"]);
    iter f seq; ()

let getDiagnosticsObject = lam info. lam msg.
  let fileInfo = getFileInfo info in
  let uri = fileInfo.filename in
  jsonKeyObject [
    ("message", JsonString msg),
    ("severity", JsonInt 1),
    ("source", JsonString "miking-lsp"),
    ("range", jsonKeyObject [
      ("start", jsonKeyObject [
        ("line", JsonInt (subi fileInfo.lineStart 1)),
        ("character", JsonInt fileInfo.colStart)
      ]),
      ("end", jsonKeyObject [
        ("line", JsonInt (subi fileInfo.lineEnd 1)),
        ("character", JsonInt fileInfo.colEnd)
      ])
    ])
  ]

let getDiagnostic = lam uri. lam errors.
  let diagnostics = map
    (lam err. getDiagnosticsObject err.0 err.1)
    errors
  in

  jsonKeyObject [
    ("jsonrpc", JsonString "2.0"),
    ("method", JsonString "textDocument/publishDiagnostics"),
    ("params", jsonKeyObject [
      ("uri", JsonString uri),
      ("diagnostics", JsonArray diagnostics)
    ])
  ]

let getDiagnostics = lam errorsGroupedByFile.
  let createDiagnosticsFromGroupedErrors = lam groupedErrors.
    getDiagnostic groupedErrors.0 groupedErrors.1
  in
  
  map createDiagnosticsFromGroupedErrors (mapToSeq errorsGroupedByFile)

-- recursive let createTypeLookup: use MExprAst in Expr -> Map Name Info = lam expr.
--   use MExprAst in
--   use MExpr in

--   let m = mapEmpty nameCmp in
--   let m = switch expr
--     case TyCon { info = Info r, ident = ident } then
--       let info = stripTempFileExtensionFromInfo (Info r) in
--       mapInsert ident info m
--     case _ then
--       m
--   end in

--   sfold_Expr_Expr (lam acc. lam expr.
--     let children = createTypeLookup expr in
--     mapUnion acc children
--   ) m expr
-- end

recursive let createDefinitionLookup: use MExprAst in Expr -> Map Name Info = lam expr.
  use MExprAst in
  use MExpr in

  let m = mapEmpty nameCmp in
  let m = switch expr
    case TmLet { ident=ident, info = Info r } then
      -- Heuristic: The info field for a let includes the entire let expression
      -- To use in LSP, we need to extract the let identifier and position.
      -- We assume that the identifier name is the first word after "let ".
      -- let info = match info with Info r then Info {
      -- 	r with
      -- 	row2 = r.row1,
      -- 	col1 = addi r.col1 (length "let "),
      -- 	col2 = addi r.col1 (length (join ["let ", nameGetStr ident]))
      -- } else info in
      let info = stripTempFileExtensionFromInfo (Info r) in
      mapInsert ident info m
    case TmLam { ty = ty, ident = ident, body = body, info = Info r } then
      let info = stripTempFileExtensionFromInfo (Info r) in
      mapInsert ident info m
    case TmRecLets { bindings = bindings } then
      let f = lam acc. lam x.
        let ident = x.ident in
        let info = stripTempFileExtensionFromInfo x.info in
        mapInsert ident info acc
      in
      foldl f m bindings
    case TmConDef { ident = ident, info = info } then
      let info = stripTempFileExtensionFromInfo info in
      mapInsert ident info m
    case _ then
      m
  end in

  
  sfold_Expr_Expr (lam acc. lam expr.
    let children = createDefinitionLookup expr in
    mapUnion acc children
  ) m expr
end

let mapInsertVariableWithFileName = lam filename. lam info. lam x. lam m.
  let filename = stripUriProtocol filename in
  match info with Info { filename = infoFilename } then
    let infoFilename = stripTempFileExtension infoFilename in
    if eqString filename infoFilename then
      mapInsert (stripTempFileExtensionFromInfo info) x m
    else
      m
  else
    m

recursive let createVariableLookup: use MExprAst in String -> Expr -> Map Info (Name, Type) =
  lam filename. lam expr.
    let mapInsertVariable = mapInsertVariableWithFileName filename in

    use MExprAst in
    use MExpr in

    let m = mapEmpty infoCmp in
    let m = switch expr
      case TmLet { ident=ident, ty=ty, info=info }
        | TmVar { ident=ident, ty=ty, info=info }
        | TmConApp { ident=ident, ty=ty, info=info }
        | TmType { ident=ident, ty=ty, info=info }
      then
        mapInsertVariable info (ident, ty) m
      case TmMatch { thn=TmVar {ident=ident}, ty=ty, info=info } then
        -- pat = PatRecord {bindings = bindings}
        mapInsertVariable info (ident, ty) m
      case TmRecLets { bindings=bindings } then
        -- The info field appears to point to just the "let" keyword??
        let f = lam acc. lam x.
          mapInsertVariable x.info (x.ident, x.tyAnnot) acc
        in
        foldl f m bindings
      case _ then m
    end in

    sfold_Expr_Expr (lam acc. lam e.
      let children = createVariableLookup filename e in
      mapUnion acc children
    ) m expr
end

recursive let createUtestLookup: use MExprAst in String -> Expr -> [Info] =
  lam filename. lam expr.
    use MExprAst in

    let filename = stripUriProtocol filename in
    let arr = [] in
    let arr = switch expr
      case TmUtest { info = info & Info r } then
        if eqString (stripTempFileExtension r.filename) filename then
          [info]
        else
          arr
      case _ then
        arr
    end in

    sfold_Expr_Expr (lam acc. lam e.
      let children = createUtestLookup filename e in
      concat acc children
    ) arr expr
end

recursive let findVariables = lam acc. lam variableLookupSeq. lam filename. lam line. lam character.
  match variableLookupSeq with [x] ++ seq then
    match x with (info, variable) in
    let collision = infoCollision info filename line character in
    let acc = if collision then
      match variable with (name, ty) in
      concat [(info, name, ty)] acc
    else
      acc
    in

    findVariables acc seq filename line character
  else
    acc
end

let getEnvironment = lam context. lam uri. lam expr.
  eprintln "Getting environment";

  -- let strippedUri = "/mnt/ProbTime/examples/coin/coin.rpl" in
  let strippedUri = stripUriProtocol uri in

  eprintln "Creating definition lookup";
  let definitionLookup = createDefinitionLookup expr in
  -- let typeLookup = createTypeLookup expr in -- TODO: implement
  (if __debug then debugPrintDefinitionLookup definitionLookup; () else ());

  eprintln "Creating variable lookup";
  let variableLookup = createVariableLookup uri expr in
  (if __debug then debugPrintVariableLookup variableLookup; () else ());

  let findVariable: String -> Int -> Int -> Option ((Info, Name, use MExprAst in Type)) = lam filename. lam line. lam character.
    let foundVariables = findVariables [] (mapToSeq variableLookup) filename line character in

    -- let seq = foundVariables in
    -- eprintln (join ["Found variables (", int2string (length seq), "):"]);
    -- let f = lam x.
    -- 	eprintln (join [info2str x.0, ": ", nameGetStr x.1]); ()
    -- in
    -- iter f seq;

    match foundVariables with [x] ++ seq then
      let f = lam var1. lam var2.
        let info1 = var1.0 in
        let info2 = var2.0 in
        if infoContainsInfo info1 info2 then var1 else var2
      in
      Some (foldl f x seq)
    else
      None ()
  in

  recursive let _findDefinition = lam definitionLookupSeq. lam name.
    match definitionLookupSeq with [x] ++ seq then
      match x with (variableName, info) in
      let collision = nameEq name variableName in
      if collision then
        Some (info)
      else
        _findDefinition seq name
    else
      None ()
  in

  let findDefinition = _findDefinition (mapToSeq definitionLookup) in
  let utestLookup = createUtestLookup uri expr in

  eprintln "Environment created!";

  {
    context.environment with
    files = mapInsert uri {
      definitionLookup = definitionLookup,

      findDefinition = findDefinition,
      findVariable = findVariable,
      utestLookup = utestLookup,

      errors = [],
      warnings = []
    } context.environment.files
  }

let getDiagnosticsResponses = lam uri. lam compilationResult.
  let errorsGroupedByFile = groupBy
    cmpString
    (lam err. (getFileInfo err.0).filename)
    compilationResult.errors
  in

  -- If there are no errors for this file, we need to include an empty list to reset the errors in the IDE.
  let emptyErrorForThisFile = mapFromSeq cmpString [(stripUriProtocol uri, [])] in
  let errorsGroupedByFile = mapUnionWith concat errorsGroupedByFile emptyErrorForThisFile in

  getDiagnostics errorsGroupedByFile

let compile = lam context. lam uri. lam content.
  let notifyPartialResult = lam compilationResult.
    let responses = getDiagnosticsResponses uri compilationResult in
    iter context.sendNotification responses;
    ()
  in

  let compilationArguments: CompilationParameters = {
    uri = uri,
    content = content,
    notifyPartialResult = notifyPartialResult
  } in

  -- Todo: use warnings in the `compilationResult`
  let compilationResult = context.compileFunc compilationArguments in
  let responses = getDiagnosticsResponses uri compilationResult in

  let environment = match compilationResult.expr with
    Some expr then
      getEnvironment context uri expr
    else
      { context.environment with files = mapRemove uri context.environment.files }
  in

  iter context.sendNotification responses;

  {
    response = None (),
    environment = environment
  }

lang LSPChange = LSPRoot
  syn Message =
  | DidChange {
    uri: String,
    version: Int,
    text: String -- todo: doesn't match LSP protocol
  }
  | DidOpen {
    uri: String,
    version: Int,
    text: String
  }
  | DidClose {
    uri: String
  }

  sem getMessage request =
  | "textDocument/didChange" ->
    match mapLookup "textDocument" request.params with Some JsonObject textDocument in
    match mapLookup "uri" textDocument with Some JsonString uri in
    match mapLookup "version" textDocument with Some JsonInt version in
    match mapLookup "contentChanges" request.params with Some JsonArray changes in
    -- only take first change, since we are requesting non-partial file changes
    match head changes with JsonObject contentChange in
    match mapLookup "text" contentChange with Some JsonString text in

    DidChange {
      uri = uri,
      version = version,
      text = text
    }
  | "textDocument/didOpen" ->
    match mapLookup "textDocument" request.params with Some JsonObject textDocument in
    match mapLookup "uri" textDocument with Some JsonString uri in
    match mapLookup "version" textDocument with Some JsonInt version in
    match mapLookup "text" textDocument with Some JsonString text in

    DidOpen {
      uri = uri,
      version = version,
      text = text
    }
  | "textDocument/didClose" ->
    match mapLookup "textDocument" request.params with Some JsonObject textDocument in
    match mapLookup "uri" textDocument with Some JsonString uri in

    DidClose {
      uri = uri
    }

  sem execute context =
  | DidOpen {uri = uri, version = version, text = text} ->
    match mapLookup uri context.environment.files with Some files then
      {
        response = None (),
        environment = context.environment
      }
    else
      compile context uri text
  | DidChange {uri = uri, version = version, text = text} ->
    compile context uri text

end