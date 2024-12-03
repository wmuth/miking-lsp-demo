include "json.mc"

-- include "./dsl.mc"
include "./json-rpc.mc"
include "./lsp/lsp.mc"
include "../lib/pprintjson.mc"

-- How long to wait before executing a batch of requests
let debounceTimeMs = 100

type MessagePruningEnvironment = {
  cancelled: Map Int Bool,
  overwrittenDidChange: Map String Bool
}

type MessageWithContext = {
  method: String,
  id: Option Int,
  message: use LSP in Message
}

-- TODO: request request pruning, e.g. removing concurrent didChange notifications
recursive let pruneMessages: MessagePruningEnvironment -> use LSP in [MessageWithContext] -> [MessageWithContext] =
  lam environment. lam messages.
    match messages with [message] ++ messages then
      use LSP in
      switch (message.id, message.message)
        case (_, CancelRequest { id = id }) then
          -- When cancelling a request, we remove the message with the same id
          -- The `CancelRequest` will during execution respond with a
          -- `ErrorCodes.RequestCancelled=32800` error, satisfying the LSP JSON-RPC spec.
          let environment = {
            environment with
            cancelled = mapInsert id true environment.cancelled
          } in
          concat [message] (pruneMessages environment messages)
        case (Some id, _) then
          let messages = pruneMessages environment messages in
          match mapLookup id environment.cancelled with Some _ then
            messages
          else
            concat [message] messages
        case (_, _) then
          concat [message] (pruneMessages environment messages)
      end
    else
      messages
end

let pruneMessages: use LSP in [MessageWithContext] -> [MessageWithContext] =
  lam messages.
    let messages = reverse messages in
    let environment: MessagePruningEnvironment = {
      cancelled = mapEmpty subi,
      overwrittenDidChange = mapEmpty cmpString
    } in
    reverse (pruneMessages environment messages)

let getMessage: String -> MessageWithContext =
  lam body.
    let jsonBody = jsonParseExn body in
    let request = getRPCRequest jsonBody in
    let method = request.method in
    let id = request.id in
    let message = use LSP in getMessage request request.method in
    {
      method = method,
      id = id,
      message = message
    }

let executeRequest: CompileFunc -> LSPEnvironment -> use LSP in Message -> LSPEnvironment =
  lam compileFunc. lam environment. lam message.
    let executionContext = {
      compileFunc = compileFunc,
      environment = environment,
      sendNotification = lam notification.
        eprintln "Sending notification\n";
        eprintln (pprintjson2string notification);
        eprintln "";
        rpcprint (json2string notification);
        ()
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

let getContentLength: String -> Int = lam header.
  match header with "Content-Length: " ++ len ++ "\n" then
    string2int len
  else
    error "The JSON-RPC header could not be read, this shouldn't happen!"

let executeRequests: CompileFunc -> LSPEnvironment -> [String] -> LSPEnvironment =
  let debugPrintMessages = lam messages.
    let messages = map
      (lam x. join [x.method, ":", match x.id with Some id then int2string id else "?"])
      messages
    in
    strJoin ", " messages
  in

  lam compileFunc. lam environment. lam bodies.
    if leqi (length bodies) 0 then
      environment
    else
      let messages = map getMessage bodies in
      let messages = pruneMessages messages in
      
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
      reduce (executeRequest compileFunc) environment messages

recursive let readJsonRPC: CompileFunc -> LSPEnvironment -> [String] -> () =
  lam compileFunc. lam environment. lam bufferedRequests.
    let headerIsReady = fileHasBytesToRead fileStdin in
    let headerIsReady = if not headerIsReady then
      sleepMs debounceTimeMs;
      fileHasBytesToRead fileStdin
    else
      headerIsReady
    in
    
    let result = if not headerIsReady then
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
              let bufferedRequests = concat bufferedRequests [asciiBody] in
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

