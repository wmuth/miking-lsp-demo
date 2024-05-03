include "json.mc"

include "../utils.mc"
include "./root.mc"

lang LSPCompletion = LSPRoot
	syn Params =
	| Completion {
		id: Int
	}

	sem getParams request =
	| "textDocument/completion" ->
		match request.id with Some id in
		Completion {
			id = id
		}

	sem execute context =
		| Completion { id = id } -> Some(
			jsonKeyObject [
				("jsonrpc", JsonString "2.0"),
				("id", JsonInt id),
				("result", jsonKeyObject [
				("isIncomplete", JsonBool true),
				("items", JsonArray [
					jsonKeyObject [
					("label", JsonString "add"),
					-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
					("kind", JsonInt 3),
					("insertText", JsonString "+"),
					("insertTextFormat", JsonInt 2),
					("documentation", jsonKeyObject [
						("kind", JsonString "markdown"),
						("value", JsonString "Addition operation `markdown test`")
					]),
					("deprecated", JsonBool true)
					],
					jsonKeyObject [
					("label", JsonString "sub"),
					("kind", JsonInt 4),
					("insertText", JsonString "-"),
					("insertTextFormat", JsonInt 2)
					],
					jsonKeyObject [
					("label", JsonString "mul"),
					("kind", JsonInt 5),
					("insertText", JsonString "*"),
					("insertTextFormat", JsonInt 2)
					],
					jsonKeyObject [
					("label", JsonString "div"),
					("kind", JsonInt 7),
					("insertText", JsonString "/"),
					("insertTextFormat", JsonInt 2)
					]
				])
				])
			]
		)
end