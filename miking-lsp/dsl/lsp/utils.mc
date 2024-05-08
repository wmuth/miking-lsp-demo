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

let stripUriProtocol = lam uri. match uri
	with "file://" ++ rest then rest
	else uri

let collision: (Int, Int) -> Int -> Bool = lam target. lam element.
	and (geqi element target.0) (leqi element target.1)

recursive let getChildExpr: use MExprAst in Expr -> Int -> Int -> Option Expr =
	lam expr. lam line. lam character.
		use MExprAst in
		sfold_Expr_Expr (lam acc. lam e.
			let info = getFileInfo (infoTm e) in
			if and (collision (info.colStart, info.colEnd) character) (collision (info.lineStart, info.lineEnd) line) then (
				match getChildExpr e line character with Some eChild then
					Some eChild
				else 
					Some e
			) else
				acc
		) (Some expr) expr
end