-- Includes from the standard library
include "common.mc"
include "mexpr/info.mc"
include "string.mc"
include "json.mc"
include "ext/file-ext.mc"

include "./ast-gen.mc"

-- Helpers to convert between floats and Exprs

let exprToFloat = use NumExprAst in
  lam e. match e with NumExpr x in x.val.v
let floatToExpr = use NumExprAst in
  lam f. NumExpr {info = NoInfo (), val = {v = f, i = NoInfo()}}


-- Language fragments implementing 'eval'

lang EvalBase = CalcBaseAst
  sem eval : Map String Expr -> Expr -> Expr
end

lang NumExprEval = EvalBase + NumExprAst
  sem eval env =
  | e & NumExpr _ -> e
end

lang TermEval = EvalBase + AddExprAst + SubExprAst
  sem eval env =
  | AddExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (addf l r)
  | SubExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (subf l r)
end

lang FactorEval = EvalBase + MulExprAst + DivExprAst
  sem eval env =
  | MulExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (mulf l r)
  | DivExpr x ->
    let l = exprToFloat (eval env x.left) in
    let r = exprToFloat (eval env x.right) in
    floatToExpr (divf l r)
end


-- Language fragments implementing 'toString'

lang ToStringBase
  sem toString : Expr -> String
end

lang NumToString = ToStringBase + NumExprAst
  sem toString =
  | NumExpr x -> float2string x.val.v
end

lang TermToString = ToStringBase + AddExprAst + SubExprAst
  sem toString =
  | AddExpr x -> join ["(", toString x.left, " + ", toString x.right, ")"]
  | SubExpr x -> join ["(", toString x.left, " - ", toString x.right, ")"]
end

lang FactorToString = ToStringBase + MulExprAst + DivExprAst
  sem toString =
  | MulExpr x -> join ["(", toString x.left, " * ", toString x.right, ")"]
  | DivExpr x -> join ["(", toString x.left, " / ", toString x.right, ")"]
end

-- Composed languages

lang Eval = TermEval + NumExprEval + FactorEval end
lang ToString = NumToString + TermToString + FactorToString end

lang Complete = CalcAst + Eval + ToString
  sem fileToExpr: File -> Expr
  sem fileToExpr =
  | File1 record -> record.e
end

-- LSP

let eprint: String -> () = lam s.
  writeString stderr s;
  flushStderr()

let eprintln: String -> () = lam s.
  writeString stderr (join [s, "\n"]);
  flushStderr()

let print: String -> () = lam s.
  writeString stdout s;
  flushStdout()

let println: String -> () = lam s.
  writeString stdout (join [s, "\n"]);
  flushStdout()

let rpcprint = lam s.
  println s

let initialResponse = "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":{\"capabilities\":{\"diagnosticProvider\":{\"interFileDependencies\":false,\"workspaceDiagnostics\":false},\"hoverProvider\":true,\"textDocumentSync\":1},\"serverInfo\":{\"name\":\"miking-lsp-server\",\"version\":\"0.1.0\"}}}"

let getFileInfo = lam fi.
  match fi with NoInfo () then
    ("", 0, 0, 0, 0)
  else match fi with Info (r & {row1 = 0}) then
    (r.filename, 0, 0, 0, 0)
  else match fi with Info r then
    (r.filename, r.row1, r.col1, r.row2, r.col2)
  else
    never

let getPublishDiagnostic = lam uri. lam version. lam diagnostics.
  JsonObject (
    mapFromSeq cmpString [
      ("jsonrpc", JsonString "2.0"),
      ("method", JsonString "textDocument/publishDiagnostics"),
      ("params", JsonObject (
        mapFromSeq cmpString [
          ("uri", JsonString uri),
          ("version", JsonInt version),
          ("diagnostics", JsonArray diagnostics)
        ]
      ))
    ]
  )

let handleDidChange = lam request.
  match request with JsonObject request in
  match mapLookup "params" request with Some JsonObject params in
  match mapLookup "textDocument" params with Some JsonObject textDocument in
  match mapLookup "uri" textDocument with Some JsonString uri in
  match mapLookup "version" textDocument with Some JsonInt version in
  match mapLookup "contentChanges" params with Some JsonArray changes in
  -- Note: We take the first element of changes since
  -- we are requesting the whole document on didChange events
  match head changes with JsonObject contentChange in
  match mapLookup "text" contentChange with Some JsonString text in
  eprintln (join ["[DidChange] { uri=", uri, ", version=", int2string version, ", text=\"", text, "\" }"]);

  switch parseCalc uri text
    case Left errors then
      let error = head errors in
      match error with (info, msg) in
      let fileInfo = getFileInfo info in
      eprintln "[Compile Failed]";

      let uri = fileInfo.0 in

      let response = getPublishDiagnostic uri version [
        JsonObject (
          mapFromSeq cmpString [
            ("message", JsonString msg),
            ("severity", JsonInt 1),
            ("source", JsonString "miking-lsp"),
            ("range", JsonObject (
              mapFromSeq cmpString [
                ("start", JsonObject (
                  mapFromSeq cmpString [
                    ("line", JsonInt (subi fileInfo.1 1)),
                    ("character", JsonInt fileInfo.2)
                  ]
                )),
                ("end", JsonObject (
                  mapFromSeq cmpString [
                    ("line", JsonInt (subi fileInfo.3 1)),
                    ("character", JsonInt fileInfo.4)
                  ]
                ))
              ]
            ))
          ]
        )
      ] in

      let responseString = json2string response in
      rpcprint responseString

    case Right file then
      eprintln "[Compile Success]";
      let response = getPublishDiagnostic uri version [] in
      let responseString = json2string response in
      rpcprint responseString
  end


let handleRequest = lam request.
  match request with JsonObject m in
    match mapLookup "method" m with Some JsonString method in
      eprintln (join ["[New request] { method=", method, " }"]);
      switch method
        case "initialize" then
          rpcprint initialResponse
        case "textDocument/didChange" then
          handleDidChange request
        case _ then
          eprintln "... [Unknown method]"
      end

recursive let readJsonRPC = lam.
  switch readLine stdin
    case None _ then {}
    case Some s then
      let json = jsonParseExn s in
      handleRequest json;
      readJsonRPC()
  end
end

mexpr
use Complete in

let emptyEnv = mapEmpty cmpString in

let printErrors = lam x.
  match x with (info, msg) in
  eprintln "We got an error";
  eprintln (infoErrorString info msg)
in

eprintln "LSP started";
readJsonRPC ();
eprintln "LSP ended"

-- parseCalcExn "example" "5 + 2. * 2.0"

-- switch parseCalc "example" "1.0 + 42.0 * 5.0"
--   case Left errors then
--     (for_ errors printErrors);
--     exit 1
--   case Right file then
--     printLn (toString (fileToExpr file))
-- end