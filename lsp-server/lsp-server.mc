include "json.mc"

-- include "./dsl.mc"
include "./json-rpc.mc"
include "./lsp/lsp.mc"
include "../lib/pprintjson.mc"

type RequestPruningEnvironment = {
  -- fileChanges: Map String String
}

-- TODO: request request pruning, e.g. removing concurrent didChange notifications
let pruneRequests: RequestPruningEnvironment -> [String] -> [String] =
  lam environment. lam requests.
    requests

let pruneRequests: [String] -> [String] =
  pruneRequests {}

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
        iter (compose eprintln pprintjson2string) response;
        eprintln "";
        iter (compose rpcprint json2string) response
      else
        eprintln ""
    );

    result.environment

let getContentLength: String -> Int = lam header.
  match header with "Content-Length: " ++ len ++ "\n" then
    string2int len
  else
    error "The JSON-RPC header could not be read, this shouldn't happen!"

let executeRequest: CompileFunc -> LSPEnvironment -> String -> LSPEnvironment =
  lam compileFunc. lam environment. lam request.
    let json = jsonParseExn request in
    handleRequest compileFunc environment json

let executeRequests: CompileFunc -> LSPEnvironment -> [String] -> LSPEnvironment =
  lam compileFunc. lam environment. lam requests.
    if leqi (length requests) 0 then
      environment
    else
      eprintln (join ["Executing ", int2string (length requests), " requests"]);
      reduce (executeRequest compileFunc) environment requests

recursive let readJsonRPC: CompileFunc -> LSPEnvironment -> [String] -> () =
  lam compileFunc. lam environment. lam bufferedRequests.
    let headerIsReady = fileHasBytesToRead fileStdin in
    let headerIsReady = if not headerIsReady then
      sleepMs 100;
      fileHasBytesToRead fileStdin
    else
      headerIsReady
    in
    
    let result = if not headerIsReady then
      -- sleepMs 100; -- Debounce
      let environment = executeRequests compileFunc environment bufferedRequests in
      let bufferedRequests = [] in
      (environment, bufferedRequests)
    else
      switch fileReadLine fileStdin
        case None _ then error "LSP client closed stdout"
        case Some header then
          -- We add 2 to the content length to account for the newline characters
          let contentHeaderLength = addi (getContentLength header) 2 in
          switch readBytesBuffered fileStdin contentHeaderLength
            case None _ then error "LSP client closed stdout"
            case Some body then
              let asciiBody = map int2char body in
              let bufferedRequests = concat [asciiBody] bufferedRequests in
              (environment, bufferedRequests)
          end
      end
    in

    let environment = result.0 in
    let bufferedRequests = result.1 in
    readJsonRPC compileFunc environment bufferedRequests
end

let readJsonRPC: CompileFunc -> LSPEnvironment -> () =
  lam compileFunc. lam environment.
    readJsonRPC compileFunc environment []

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

