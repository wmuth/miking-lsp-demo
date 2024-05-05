include "mexpr/info.mc"

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

type ProdPosition = {
  filename: String,
  lineStart: Int,
  colStart: Int,
  lineEnd: Int,
  colEnd: Int
}

let getFileInfo: Info -> ProdPosition = lam fi.
  match fi with NoInfo () then
    {
      filename = "",
      lineStart = 0,
      colStart = 0,
      lineEnd = 0,
      colEnd = 0
    }
  else match fi with Info (r & {row1 = 0}) then
    {
      filename = r.filename,
      lineStart = 0,
      colStart = 0,
      lineEnd = 0,
      colEnd = 0
    }
  else match fi with Info r then
    {
      filename = r.filename,
      lineStart = r.row1,
      colStart = r.col1,
      lineEnd = r.row2,
      colEnd = r.col2
    }
  else
    never