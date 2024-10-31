include "json.mc"

include "./dsl.mc"
include "./json-rpc.mc"
include "./lsp/lsp.mc"

let handleRequest = lam compileFunc. lam request.
  let request = getRPCRequest request in
  let method = request.method in
  eprintln method;
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

let getContentLength: String -> Int = lam header.
  match header with "Content-Length: " ++ len ++ "\n" then
    string2int len
  else
    error "Content-Length not found"

recursive let readJsonRPC = lam compileFunc.
  switch readLine stdin  
    case None _ then {}
    case Some header then
      switch readBytesBuffered stdin (addi (getContentLength header) 1) -- We add 1 to the content length to account for the newline characters
        case None _ then {}
        case Some body then
          let json = jsonParseExn body in
          handleRequest compileFunc json;
          readJsonRPC compileFunc
      end
  end
end

mexpr

let compileFunc: use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations) =
  lam uri. lam code.
    use Complete in
    switch parseCalc uri code
      case Right file then
        let context = {} in
        let lspResult = (stmtToLSP context (fileToStatements file)) in
        let implementations = foldl (
          lam acc. lam x.
            {
              hover=join [acc.hover, x.hover]
            }
        ) lsp lspResult in
        let expr = compileStatementsToMexpr (fileToStatements file) in
        Right (expr, implementations)
      case Left errors then
        Left errors
    end
in

-- let compileFunc: use MExprAst in String -> String -> Either [(Info, String)] Expr =
--   lam uri. lam code.
--     use Complete in
--     switch parseCalc uri code
--       case Right file then
--         Right (compileStatementsToMexpr (fileToStatements file))
--       case Left errors then
--         Left errors
--     end
-- in

eprintln "Miking LSP started";
readJsonRPC compileFunc;
eprintln "Miking LSP ended"