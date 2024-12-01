include "json.mc"

include "./dsl.mc"
include "./json-rpc.mc"
include "./lsp/lsp.mc"
include "./lsp/pprintjson.mc"

let handleRequest = lam compileFunc. lam environment. lam request.
  let request = getRPCRequest request in
  let method = request.method in
  eprintln method;
  
  use LSP in
  let params = getParams request request.method in
  match params with UnknownMethod {} then
    eprintln (join ["[Unknown method] ", method]);
    environment
  else
    let executionContext = {
      compileFunc = compileFunc,
      environment = environment
    } in
    
    let result = execute executionContext params in

    (
      match result.response with Some response then
        eprintln "Responding to request\n";
        eprintln (pprintjson2string response);
        eprintln "";

        rpcprint (json2string response)
      else
        eprintln ""
    );

    result.environment


let getContentLength: String -> Int = lam header.
  match header with "Content-Length: " ++ len ++ "\n" then
    string2int len
  else
    error "Content-Length not found"

recursive let readJsonRPC = lam compileFunc. lam environment.
  switch fileReadLine fileStdin
    case None _ then {}
    case Some header then
      -- We add 2 to the content length to account for the newline characters
      let contentHeaderLength = addi (getContentLength header) 2 in
      switch readBytesBuffered fileStdin contentHeaderLength
        case None _ then {}
        case Some body then
          let asciiBody = map int2char body in
          let json = jsonParseExn asciiBody in
          let environment = handleRequest compileFunc environment json in
          readJsonRPC compileFunc environment
      end
  end
end

mexpr

-- let compileFunc: use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations) =
--   lam uri. lam code.
--     use Complete in
--     switch parseCalc uri code
--       case Right file then
--         let context = {} in
--         let lspResult = (stmtToLSP context (fileToStatements file)) in
--         let implementations = foldl (
--           lam acc. lam x.
--             {
--               hover=join [acc.hover, x.hover]
--             }
--         ) lsp lspResult in
--         let expr = compileStatementsToMexpr (fileToStatements file) in
--         Right (expr, implementations)
--       case Left errors then
--         Left errors
--     end
-- in

-- -- let compileFunc: use MExprAst in String -> String -> Either [(Info, String)] Expr =
-- --   lam uri. lam code.
-- --     use Complete in
-- --     switch parseCalc uri code
-- --       case Right file then
-- --         Right (compileStatementsToMexpr (fileToStatements file))
-- --       case Left errors then
-- --         Left errors
-- --     end
-- -- in

-- let environment: LSPEnvironment = {
--   files = mapEmpty cmpString
-- } in

-- eprintln "Miking LSP started";
-- readJsonRPC compileFunc environment;
-- eprintln "Miking LSP ended"

()