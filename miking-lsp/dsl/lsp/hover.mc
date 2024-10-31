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
			-- Add 1 to incoming line and character to match the 1-based indexing of LSP
			let line = addi line 1 in
			let strippedUri = stripUriProtocol uri in
			let content = readFile strippedUri in

			match context.compileFunc strippedUri content with Right (expr, implementations) then
				let debugText = join [
					"Uri: ", uri, ", Line: ", int2string line, ", Character: ", int2string character, "\n\n"
				] in

				let hoverInformation = (
					strJoin ", " (map 
						(lam x.
							let info = getFileInfo x.info in
							join [
								"<",
								x.content,
								" @ ",
								int2string (subi info.lineStart 1),
								":",
								int2string info.colStart,
								"-",
								int2string (subi info.lineEnd 1),
								":",
								int2string info.colEnd,
								">"
							]
						) implementations.hover
					)
				) in

				-- eprintln hoverInformation;
				-- None ()

				let result = match getChildExpr expr line character with Some expr then
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