include "json.mc"

include "../utils.mc"
include "./utils.mc"

include "./root.mc"

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

lang LSPHover = LSPRoot
	syn Params =
	| Hover {
		id: Int,
		textDocument: TextDocumentPositionParams
	}

	sem getParams request =
	| "textDocument/hover" ->
		match request.id with Some id in
		Hover {
			id = id,
			textDocument = getTextDocumentPositionParams request.params
		}

	sem execute context =
		| Hover { id = id, textDocument = {
			uri = uri,
			line = line,
			character = character
		} } -> 
			-- Add 1 to incoming line and character to match the 1-based indexing of LSP
			let line = addi line 1 in
			let strippedUri = stripUriProtocol uri in
			let content = readFile strippedUri in

			match context.compileFunc strippedUri content with Right mexprAst then
				let debugText = join [
					"Uri: ", uri, ", Line: ", int2string line, ", Character: ", int2string character, "\n\n"
				] in

				let result = match getChildExpr mexprAst line character with Some expr then
					use MExprPrettyPrint in
					let info = getFileInfo (infoTm expr) in
					jsonKeyObject [
						("contents", JsonString (join [
							debugText,
							expr2str expr,
							"\n\n",
							"Type: ", type2str (tyTm expr)
						])),
						("range", jsonKeyObject [
							("start", jsonKeyObject [
								("line", JsonInt (subi info.lineStart 1)),
								("character", JsonInt info.colStart)
							]),
							("end", jsonKeyObject [
								("line", JsonInt (subi info.lineEnd 1)),
								("character", JsonInt info.colEnd)
							])
						])
					]
				else
					jsonKeyObject [
						("contents", JsonString (join [
							debugText,
							"[nothing found]"
						]))
					]
				in

				Some(jsonKeyObject [
					("jsonrpc", JsonString "2.0"),
					("id", JsonInt id),
					("result", result)
				])
			else
				eprintln "Error parsing file";

				Some(jsonKeyObject [
					("jsonrpc", JsonString "2.0"),
					("id", JsonInt id),
					("result", jsonKeyObject [
						("contents", JsonString "Error parsing file")
					])
				])

end