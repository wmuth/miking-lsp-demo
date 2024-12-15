include "json.mc"

include "../lib/pprintjson.mc"

include "./json-rpc.mc"
include "./lsp/lsp.mc"
include "./prune.mc"

-- How long to wait before executing a batch of requests
let debounceTimeMs = 10

let executeRequest: LSPStartParameters -> LSPEnvironment -> use LSP in Message -> LSPEnvironment =
  lam parameters. lam environment. lam message.
    let executionContext = {
      parameters = parameters,
      environment = environment,
      sendNotification = lam notification.
        eprintln "Sending notification\n";
        eprintln (pprintjson2string notification);
        eprintln "";
        rpcprint (json2string notification)
    } in
    
    let result = use LSP in execute executionContext message in

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

let executeRequests: LSPStartParameters -> LSPEnvironment -> [String] -> LSPEnvironment =
  lam parameters. lam environment. lam bodies.
    let debugPrintMessages = lam messages.
      let messages = map
        (lam x. join [x.method, ":", match x.id with Some id then int2string id else "?"])
        messages
      in
      strJoin ", " messages
    in

    if leqi (length bodies) 0 then
      environment
    else
      let messages = map getMessage bodies in
      let messages = if parameters.options.pruneMessages
        then pruneMessages messages
        else messages
      in
      
      eprintln (join [
        "Executing ",
        int2string (length messages),
        " requests (",
        int2string (subi (length bodies) (length messages)),
        " pruned): [",
        debugPrintMessages messages,
        "]"
      ]);

      let messages = map (lam message. message.message) messages in
      reduce (executeRequest parameters) environment messages

recursive let readJsonRPC: LSPStartParameters -> LSPEnvironment -> [String] -> () =
  lam parameters. lam environment. lam bufferedRequests.
    let headerIsReady = fileHasBytesToRead fileStdin in
    let headerIsReady = if not headerIsReady then
      sleepMs debounceTimeMs;
      fileHasBytesToRead fileStdin
    else
      headerIsReady
    in
    
    let result = if not headerIsReady then
      let environment = executeRequests parameters environment bufferedRequests in
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
              let bufferedRequests = concat bufferedRequests [asciiBody] in
              (environment, bufferedRequests)
          end
      end
    in

    let environment = result.0 in
    let bufferedRequests = result.1 in
    readJsonRPC parameters environment bufferedRequests
end

let startLSPServer: LSPStartParameters -> () =
  lam parameters.
    let environment: LSPEnvironment = {
      files = mapEmpty cmpString
    } in
    readJsonRPC parameters environment []
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

