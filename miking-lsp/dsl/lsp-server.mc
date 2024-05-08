include "json.mc"

include "./dsl.mc"
include "./json-rpc.mc"
include "./lsp/lsp.mc"

let handleRequest = lam compileFunc. lam request.
  let request = getRPCRequest request in
  let method = request.method in
  use LSP in
  let params = getParams request request.method in
  match params with UnknownMethod {} then
    eprintln (join ["[Unknown method] ", method])
  else
    let executionContext = {
      compileFunc = compileFunc
    } in
    let response = execute executionContext params in
    match response with Some response then
      rpcprint (json2string response)
    else
      eprintln ""

recursive let readJsonRPC = lam compileFunc.
  switch readLine stdin
    case None _ then {}
    case Some s then
      let json = jsonParseExn s in
      handleRequest compileFunc json;
      readJsonRPC compileFunc
  end
end

mexpr

let compileFunc: use MExprAst in String -> String -> Either [(Info, String)] Expr =
  lam uri. lam code.
    use Complete in
    switch parseCalc uri code
      case Right file then
        Right (compileToMexpr (fileToExpr file))
      case Left errors then
        Left errors
    end
in

eprintln "Miking LSP started";
readJsonRPC compileFunc;
eprintln "Miking LSP ended"