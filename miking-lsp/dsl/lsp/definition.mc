include "json.mc"

include "../utils.mc"
include "./utils.mc"

include "./root.mc"

lang LSPGotoDefinition = LSPRoot
	syn Params =
	| GotoDefinition {
		id: Int,
		textDocument: TextDocumentPositionParams
	}

	sem getParams request =
	| "textDocument/definition" ->
		match request.id with Some id in
		GotoDefinition {
			id = id,
			textDocument = getTextDocumentPositionParams request.params
		}

	sem execute context =
		| GotoDefinition { id = id, textDocument = {
			uri = uri,
			line = line,
			character = character
		} } -> 
			-- Add 1 to incoming line and character to match the 1-based indexing of LSP
			let line = addi line 1 in
			let strippedUri = stripUriProtocol uri in
			let content = readFile strippedUri in

			Some(jsonKeyObject [
				("jsonrpc", JsonString "2.0"),
				("id", JsonInt id),
				("result", jsonKeyObject [
					("uri", JsonString strippedUri),
					("range", jsonKeyObject [
						("start", jsonKeyObject [
							("line", JsonInt 0),
							("character", JsonInt 0)
						]),
						("end", jsonKeyObject [
							("line", JsonInt 0),
							("character", JsonInt 0)
						])
					])
				])
			])

end