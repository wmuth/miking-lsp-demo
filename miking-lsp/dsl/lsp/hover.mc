include "json.mc"

include "../utils.mc"
include "./utils.mc"

include "./root.mc"

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
			use Complete in

			-- Add 1 to incoming line and character to match the 1-based indexing of LSP
			let line = addi line 1 in

			let stripUriProtocol = lam uri. match uri
				with "file://" ++ rest then rest
				else uri
			in
			let strippedUri = stripUriProtocol uri in
			let content = readFile strippedUri in

			match parseCalc strippedUri content with Right file then
				let expr = fileToExpr file in
				let emptyEnv = mapEmpty cmpString in
				let exprInfo = get_Expr_info expr in

				let collision: (Int, Int) -> Int -> Bool = lam target. lam element.
					and (geqi element target.0) (leqi element target.1)
				in

				recursive let countExprs
					: Expr -> Int = lam expr.
						sfold_Expr_Expr (lam count. lam e. addi count (countExprs e)) 1 expr
				in

				recursive let getChildExpr
					: Expr -> Int -> Int -> Option Expr = lam expr. lam line. lam character.
						sfold_Expr_Expr (lam acc. lam e.
							let info = getFileInfo (get_Expr_info e) in
							if and (collision (info.colStart, info.colEnd) character) (collision (info.lineStart, info.lineEnd) line) then (
								eprintln "Collision";
								match getChildExpr e line character with Some eChild then
									Some eChild
								else 
									Some e
							) else
								eprintln "No collision";
								acc
						) (Some expr) expr
				in

				let exprs = countExprs expr in
				let expr = match getChildExpr expr line character with Some childExpr then
					toString childExpr
				else
					"[nothing found]"
				in

				let debugText = join [
					"Uri: ", uri, ", Line: ", int2string line, ", Character: ", int2string character
				] in

				let text = join [
					debugText, "\n\n",
					"Expression: ", expr, "\n\n",
					"len exprs: ", int2string exprs
				] in

				Some(jsonKeyObject [
					("jsonrpc", JsonString "2.0"),
					("id", JsonInt id),
					("result", jsonKeyObject [
						("contents", JsonString text)
					])
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