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
					-- "Uri: ", uri, ", ",
					"Line: ", int2string line, ", Character: ", int2string character, "\n\n"
				] in

				-- let hoverInformation = (
				-- 	strJoin ", " (map 
				-- 		(lam x.
				-- 			let info = getFileInfo x.info in
				-- 			join [
				-- 				"<",
				-- 				x.content,
				-- 				" @ ",
				-- 				int2string (subi info.lineStart 1),
				-- 				":",
				-- 				int2string info.colStart,
				-- 				"-",
				-- 				int2string (subi info.lineEnd 1),
				-- 				":",
				-- 				int2string info.colEnd,
				-- 				">"
				-- 			]
				-- 		) implementations.hover
				-- 	)
				-- ) in

				-- eprintln hoverInformation;
				-- None ()

				use MExpr in
				let expr = symbolize expr in

				eprintln (join ["Symbolized: ", use MExprPrettyPrint in expr2str expr, "\n"]);

				recursive let createDefinitionLookup: use MExprAst in Expr -> Map Name Info =
					lam expr.
						use MExprAst in

						let m = mapEmpty nameCmp in

						let m = match expr with
							TmLet { ident=ident, info=info } then mapInsert ident info m
							else m
						in

						sfold_Expr_Expr (lam acc. lam e.
							let children = createDefinitionLookup e in
							mapUnion acc children
						) m expr
				in

				let astLookup = createDefinitionLookup expr in
				let seq = mapToSeq astLookup in
				let f = lam x. eprintln (join [nameGetStr x.0, ": ", info2str x.1]); () in
				iter f seq;

				recursive let createVariableLookup: use MExprAst in Expr -> Map Info (Name, Type) = 
					lam expr.
						use MExprAst in

						let m = mapEmpty infoCmp in

						let m = match expr with
							TmVar { ident=ident, ty=ty, info=info } then mapInsert info (ident, ty) m
							else m
						in

						sfold_Expr_Expr (lam acc. lam e.
							let children = createVariableLookup e in
							mapUnion acc children
						) m expr
				in

				let variableLookup = createVariableLookup expr in
				let seq = mapToSeq variableLookup in
				let f = lam x.
					let v = x.1 in
					eprintln (join [info2str x.0, ": ", nameGetStr v.0, "(ty:", type2str v.1, ")"]); () in
				iter f seq;

				recursive let findVariable = lam line. lam character. lam variableLookupSeq.
					match variableLookupSeq with [x] ++ seq then
						let info = x.0 in
						let variable = x.1 in
						let collision = infoCollision info line character in
						if collision then
							Some (info, variable)
						else
							findVariable line character seq
					else None ()
				in

				let variable = findVariable line character (mapToSeq variableLookup) in

				let result = match variable with Some (info, variable) then
					eprintln (join ["Variable: ", nameGetStr variable.0, "(ty:", type2str variable.1, ")\n"]);
					let info = getFileInfo info in
					jsonKeyObject [
						("contents", JsonString (join [
							debugText,
							-- expr2str expr,
							"\n\n",
							join [
								"Variable: ", nameGetStr variable.0, " <", type2str variable.1, ">"
							]
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
					eprintln "Variable not found";
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