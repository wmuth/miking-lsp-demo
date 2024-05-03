type TextDocumentPositionParams = {
  uri: String,
  line: Int,
  character: Int
}

let getTextDocumentPositionParams: Map String JsonValue -> TextDocumentPositionParams = lam params.
  match mapLookup "textDocument" params with Some JsonObject textDocument in
  match mapLookup "uri" textDocument with Some JsonString uri in
  match mapLookup "position" params with Some JsonObject position in
  match mapLookup "line" position with Some JsonInt line in
  match mapLookup "character" position with Some JsonInt character in
  {
    uri = uri,
    line = line,
    character = character
  }