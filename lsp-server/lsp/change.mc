include "json.mc"
include "mexpr/type-check.mc"
include "mexpr/mexpr.mc"
include "mexpr/keyword-maker.mc"

include "../../lib/utils.mc"
include "./utils.mc"
include "./root.mc"
include "./diagnostic.mc"
include "./compile.mc"

-- Print debug information
let __debug = false

let emptyResponse = lam context. {
  response = None (),
  environment = context.environment
}

lang LSPChange = LSPRoot + LSPCompileUtility
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
    eprintln (join ["Closed: ", uri]);
    context.parameters.onClose uri;
    emptyResponse context
  | DidOpen {uri = uri, version = version, text = text} ->
    eprintln (join ["Opened: ", uri]);
    {
      response = None (),
      environment = handleCompile context uri text context.parameters.onOpen
    }
  | DidChange {uri = uri, version = version, text = text} ->
    eprintln (join ["Changed: ", uri]);
    {
      response = None (),
      environment = handleCompile context uri text context.parameters.onChange
    }

end