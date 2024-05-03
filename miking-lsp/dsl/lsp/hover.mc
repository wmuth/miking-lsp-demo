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

			let text = join [
				"Uri: ", uri, ", Line: ", int2string line, ", Character: ", int2string character
			] in

			let stripUriProtocol = lam uri. match uri
				with "file://" ++ rest then rest
				else uri
			in
			let strippedUri = stripUriProtocol uri in
			let content = readFile strippedUri in

			match parseCalc strippedUri content with Right file then
				let expr = fileToExpr file in
				let emptyEnv = mapEmpty cmpString in
				let representation = toString expr in
				-- eprintln (toString (eval emptyEnv expr));
				-- eprintln (toString expr);

				-- let info = get_File_info file in
				-- let a = match info with Info r then
				-- 	eprintln r.filename;
				-- 	()
				-- else
				-- 	eprintln "No info";
				-- 	()
				-- in

				Some(jsonKeyObject [
					("jsonrpc", JsonString "2.0"),
					("id", JsonInt id),
					("result", jsonKeyObject [
						("contents", JsonString representation)
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