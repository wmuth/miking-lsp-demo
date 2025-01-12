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

  sem handleCompile: LSPExecutionContext -> URI -> String -> (CompilationParameters -> Map URI [LanguageServerPayload]) -> LSPResult
  sem handleCompile context uri content =| compilationFunction ->
    let compilationParameters: CompilationParameters = {
      uri = uri,
      content = content
    } in
  
    let filePayloads: Map URI [LanguageServerPayload] = compilationFunction compilationParameters in
    let compilationResults: Map URI LanguageServerContext = mapMap (
      foldl populateContext emptyLanguageServerContext
    ) filePayloads in
  
    let responses = getResultResponses compilationResults in
    iter context.sendNotification responses;
  
    {
      response = None (),
      environment = {
        context.environment with
        files = mapUnion context.environment.files compilationResults
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
    handleCompile context uri text context.parameters.onOpen
  | DidChange {uri = uri, version = version, text = text} ->
    handleCompile context uri text context.parameters.onChange

end