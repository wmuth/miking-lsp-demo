include "json.mc"
include "mexpr/type-check.mc"
include "mexpr/mexpr.mc"
include "mexpr/keyword-maker.mc"

include "../../lib/utils.mc"
include "./utils.mc"
include "./root.mc"
include "./diagnostic.mc"

-- Print debug information
let __debug = false

let emptyResponse = lam context. {
  response = None (),
  environment = context.environment
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

  sem handleCompile: LSPExecutionContext -> URI -> String -> (CompilationParameters -> CompilationResult) -> (URI -> [LanguageServerPayload]) -> LSPResult
  sem handleCompile context uri content compilationFunction =| languageSupportFunction ->
    let files = context.environment.files in

    let compilationParameters: CompilationParameters = {
      uri = uri,
      content = content
    } in
  
    let compilationResults: CompilationResult = compilationFunction compilationParameters in
    let dependencies: [URI] = compilationResults.dependencies in
    let filePayloads: Map URI [LanguageServerPayload] = compilationResults.languageSupport in

    let compilationResults: Map URI LanguageServerContext = mapMap (
      foldl populateContext emptyLanguageServerContext
    ) filePayloads in
  
    let responses = getResultResponses compilationResults in
    iter context.sendNotification responses;

    let files = mapUnion files compilationResults in

    -- Handle dependencies which have not yet been included
    let nonExistingDependencies = filter (lam dependency. not (mapMem dependency files)) dependencies in
    let dependencyFiles = mapFromSeq cmpString (map (lam path. (path, languageSupportFunction path)) nonExistingDependencies) in
    -- eprintln (join ["Non-existing dependencies: ", strJoin ", " nonExistingDependencies]);
    let dependencyCompilationResults: Map URI LanguageServerContext = mapMap (
      foldl populateContext emptyLanguageServerContext
    ) dependencyFiles in

    let files = mapUnion files dependencyCompilationResults in
  
    {
      response = None (),
      environment = {
        context.environment with
        files = files
      }
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
  | DidClose { uri = uri } ->
    context.parameters.onClose uri;
    {
      response = None (),
      environment = { context.environment with files = mapRemove uri context.environment.files }
    }
  | DidOpen {uri = uri, version = version, text = text} ->
    eprintln (join ["Opened: ", uri]);
    handleCompile context uri text context.parameters.onOpen context.parameters.getLanguageSupport
  | DidChange {uri = uri, version = version, text = text} ->
    eprintln (join ["Changed: ", uri]);
    handleCompile context uri text context.parameters.onChange context.parameters.getLanguageSupport

end