include "json.mc"

include "../utils.mc"
include "./root.mc"

let getFileInfo = lam fi.
	match fi with NoInfo () then
		("", 0, 0, 0, 0)
	else match fi with Info (r & {row1 = 0}) then
		(r.filename, 0, 0, 0, 0)
	else match fi with Info r then
		(r.filename, r.row1, r.col1, r.row2, r.col2)
	else
		never

let getPublishDiagnostic = lam uri. lam version. lam diagnostics.
	jsonKeyObject [
		("jsonrpc", JsonString "2.0"),
		("method", JsonString "textDocument/publishDiagnostics"),
		("params", jsonKeyObject [
		("uri", JsonString uri),
		("version", JsonInt version),
		("diagnostics", JsonArray diagnostics)
		])
	]

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
		-- only take first change, since we are requesting non-partial file changes
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
		-- switch context.parseFunc uri text
		switch parseCalc uri text
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