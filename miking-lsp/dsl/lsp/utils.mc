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

let infoCollision: Info -> String -> Int -> Int -> Bool = lam info. lam filename. lam line. lam character.
  match info with Info r then
    (
      and
      (
        eqString (stripUriProtocol r.filename) (stripUriProtocol filename)
      )
      (
        and
        (collision (r.row1, r.row2) line)
        (collision (r.col1, r.col2) character)
      )
    )
  else
    false

let infoContainsInfo: Info -> Info -> Bool = lam info1. lam info2.
  match info1 with Info r1 then
    match info2 with Info r2 then
      (
        and
        (
          eqString r1.filename r2.filename
        )
        (
          and 
          (and (geqi r1.row1 r2.row1) (leqi r1.row2 r2.row2))
          (and (geqi r1.col1 r2.col1) (leqi r1.col2 r2.col2))
        )
      )
    else
      false
  else
    false

-- Try to do it binary search, but it's not working
recursive let getChildExpr: use MExprAst in Expr -> Int -> Int -> Option Expr =
	lam expr. lam line. lam character.
    -- eprintln (join ["Looking for line: ", int2string line, ", character: ", int2string character, "\n"]);
    -- eprintln (join ["Starting at:\n--------\n", use MExprPrettyPrint in expr2str expr, "\n-------\n"]);

		use MExprAst in
		sfold_Expr_Expr (lam acc. lam e.
			let info = getFileInfo (infoTm e) in
      -- eprintln (join ["Looking at: line:", int2string info.lineStart, ", character:", int2string info.colStart, " to line:", int2string info.lineEnd, ", character:", int2string info.colEnd ,"\n--------\n", use MExprPrettyPrint in expr2str e, "\n-------\n"]);
			if and (collision (info.colStart, info.colEnd) character) (collision (info.lineStart, info.lineEnd) line) then (
				match getChildExpr e line character with Some eChild then
          -- eprintln (join ["Found:\n---------\n", use MExprPrettyPrint in expr2str eChild, "\n-------\n"]);
					Some eChild
				else 
          -- eprintln "No child found\n";
					Some e
			) else
        -- eprintln "No collision\n";
				acc
		) (Some expr) expr
end