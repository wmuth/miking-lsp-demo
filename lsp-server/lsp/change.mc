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

let handleCompile = lam context. lam uri. lam content. lam compilationFunction.
  let notify: URI -> CompilationDiagnostics -> () =
    lam uri. lam notification.
      let response = getResultResponses (mapFromSeq cmpString [(uri, notification)]) in
      iter context.sendNotification response
  in

  let compilationParameters: CompilationParameters = {
    uri = uri,
    content = content,
    notify = notify
  } in

  let compilationResults: Map URI CompilationResult = compilationFunction compilationParameters in
  let compilationDiagnostics: Map URI CompilationDiagnostics = mapMap (
    lam v. {
      errors = v.errors,
      warnings = v.warnings
    }
  ) compilationResults in

  let responses = getResultResponses compilationDiagnostics in
  iter context.sendNotification responses;

  let newFiles: [(URI, LSPFileEnvironment)] = map (lam compilationResult.
    match compilationResult with (uri, compilationResult) in
    (
      uri,
      {
        lookup = compilationResult.lookup,
        lenses = compilationResult.lenses
      }
    )
  ) (mapToSeq compilationResults) in

  let newFiles = mapFromSeq cmpString newFiles in

  {
    response = None (),
    environment = {
      context.environment with
      files = mapUnion context.environment.files newFiles
    }
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