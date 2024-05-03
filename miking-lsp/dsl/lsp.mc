include "json.mc"

include "./utils.mc"

type ParseFunc = String -> String -> Either [(Info, String)] [(Info, String)]

type LSPExecutionContext = {
	parseFunc: ParseFunc
}

lang LSPRoot
	syn Params =
	sem getParams: RPCRequest -> String -> Params
	sem execute: LSPExecutionContext -> Params -> Option JsonValue -- todo: return abstracted LSP response
end

lang LSPInitialize = LSPRoot
	syn Params =
	| Initialized {}
	| Initialize { }

	sem getParams request =
	| "initialized" ->
		Initialized {}
	| "initialize" ->
		Initialize {}

	sem execute context =
	| Initialized {} -> None ()
	| Initialize {} -> Some (
		jsonKeyObject [
			("jsonrpc", JsonString "2.0"),
			("id", JsonInt 0),
			("result", jsonKeyObject [
				("capabilities", jsonKeyObject [
					("diagnosticProvider", jsonKeyObject [
						("interFileDependencies", JsonBool false),
						("workspaceDiagnostics", JsonBool false)
					]),
					("hoverProvider", JsonBool true),
					("textDocumentSync", JsonInt 1),
					("completionProvider", jsonKeyObject [
						("triggerCharacters", JsonArray [
							JsonString "."
						])
					])
				]),
				("serverInfo", jsonKeyObject [
					("name", JsonString "miking-lsp-server"),
					("version", JsonString "0.1.0")
				])
			])
		]
	)
end

lang LSPDiagnostics = LSPRoot
	syn Params =
	| DidChange {
		textDocument: {
			uri: String,
			version: Int
		},
		text: String -- todo: doesn't match LSP protocol
	}

	sem getParams request =
	| "textDocument/didChange" ->
		match mapLookup "textDocument" request.params with Some JsonObject textDocument in
		match mapLookup "uri" textDocument with Some JsonString uri in
		match mapLookup "version" textDocument with Some JsonInt version in
		match mapLookup "contentChanges" request.params with Some JsonArray changes in
		match head changes with JsonObject contentChange in
		match mapLookup "text" contentChange with Some JsonString text in

		DidChange {
			textDocument = {
				uri = uri,
				version = version
			},
			text = text
		}

	sem execute context =
	| DidChange {textDocument = {uri = uri, version = version}, text = text} ->
		let getFileInfo = lam fi.
			match fi with NoInfo () then
				("", 0, 0, 0, 0)
			else match fi with Info (r & {row1 = 0}) then
				(r.filename, 0, 0, 0, 0)
			else match fi with Info r then
				(r.filename, r.row1, r.col1, r.row2, r.col2)
			else
				never
			in

		let getPublishDiagnostic = lam uri. lam version. lam diagnostics.
			jsonKeyObject [
				("jsonrpc", JsonString "2.0"),
				("method", JsonString "textDocument/publishDiagnostics"),
				("params", jsonKeyObject [
				("uri", JsonString uri),
				("version", JsonInt version),
				("diagnostics", JsonArray diagnostics)
				])
			] in

		switch context.parseFunc uri text
			case Left errors then
				let error = head errors in
				match error with (info, msg) in
				let fileInfo = getFileInfo info in
				eprintln "[Compile Failed]";
			
				let uri = fileInfo.0 in
			
				let response = getPublishDiagnostic uri version [
					jsonKeyObject [
						("message", JsonString msg),
						("severity", JsonInt 1),
						("source", JsonString "miking-lsp"),
						("range", jsonKeyObject [
							("start", jsonKeyObject [
								("line", JsonInt (subi fileInfo.1 1)),
								("character", JsonInt fileInfo.2)
							]),
							("end", jsonKeyObject [
								("line", JsonInt (subi fileInfo.3 1)),
								("character", JsonInt fileInfo.4)
							])
						])
					]
				] in
			
				Some(response)
		
			case Right file then
				eprintln "[Compile Success]";
				let response = getPublishDiagnostic uri version [] in
				Some(response) 
		end

end

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

lang LSPUnknownMethod = LSPRoot
	syn Params =
	| UnknownMethod {}

	sem getParams request =
	| _method ->
		UnknownMethod {}

	sem execute context =
	| UnknownMethod {} -> None ()
end

lang LSP =
LSPInitialize
+ LSPDiagnostics
+ LSPCompletion
+ LSPUnknownMethod
end