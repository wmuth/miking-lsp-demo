include "./root.mc"
include "./diagnostic.mc"

lang LSPCompileUtility = LSPRoot
  -- TODO: Keep track of what diagnostics we have sent, so that we don't spam the client
  -- with the same diagnostics over and over again.
  sem handleCompile: EventType -> LSPExecutionContext -> URI -> String -> (LSPCompilationParameters -> LSPCompilationResult) -> LSPEnvironment
  sem handleCompile eventType context uri content =| compilationFunction ->
    let files = context.environment.files in

    let compilationParameters: LSPCompilationParameters = {
      uri = uri,
      content = content,
      typ = eventType
    } in
  
    let compilationResults: Map URI [LanguageServerPayload]  = compilationFunction compilationParameters in
    let compilationResults: Map URI LanguageServerContext = mapMap (foldl populateContext emptyLanguageServerContext) compilationResults in
  
    let responses = getResultResponses context compilationResults in
    iter context.sendNotification responses;

    let newFiles: [(URI, LanguageServerContext)] = map (lam compilationResult.
      match compilationResult with (uri, context) in (uri, context)
    ) (mapToSeq compilationResults) in
  
    let newFiles = mapFromSeq cmpString newFiles in
  
    {
      context.environment with
      files = mapUnion context.environment.files newFiles
    }
end