include "json.mc"

include "../utils.mc"
include "./utils.mc"

include "./root.mc"
include "../mexpr.mc"

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

			match context.compileFunc strippedUri content with Right (expr, implementations) then
				use MExprLSP in
				let definitionTree = buildDefinitionTree expr in

				match getChildExpr expr line character with Some exprNode then
					use MExprPrettyPrint in
					eprintln (join ["Found exprNode:", expr2str exprNode]);

					match exprNode with TmVar { ident=ident } then
						match (mapLookup ident.0 definitionTree) with Some info then
							eprintln (join ["Found a:", info2str info]);
							let info = getFileInfo info in
	
							Some(jsonKeyObject [
								("jsonrpc", JsonString "2.0"),
								("id", JsonInt id),
								("result", jsonKeyObject [
									("uri", JsonString strippedUri),
									("range", jsonKeyObject [
										("start", jsonKeyObject [
											("line", JsonInt (subi info.lineStart 1)),
											("character", JsonInt info.colStart)
										]),
										("end", jsonKeyObject [
											("line", JsonInt (subi info.lineEnd 1)),
											("character", JsonInt info.colEnd)
										])
										-- ("start", jsonKeyObject [
										-- 	("line", JsonInt 0),
										-- 	("character", JsonInt 0)
										-- ]),
										-- ("end", jsonKeyObject [
										-- 	("line", JsonInt 0),
										-- 	("character", JsonInt 0)
										-- ])
									])
								])
							])
						else 
							None ()
					else
						None ()
				else 
					None ()
			else
				eprintln "Error parsing file";
				None ()

end