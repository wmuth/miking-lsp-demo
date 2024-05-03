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
		} } -> Some(
			let text = join [
				"Uri: ", uri, ", Line: ", int2string line, ", Character: ", int2string character
			] in
			jsonKeyObject [
				("jsonrpc", JsonString "2.0"),
				("id", JsonInt id),
				("result", jsonKeyObject [
					("contents", JsonString text)
				])
			]
		)
end