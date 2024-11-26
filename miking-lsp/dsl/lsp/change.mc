include "json.mc"

include "../utils.mc"
include "./utils.mc"
include "./root.mc"

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

lang LSPChange = LSPRoot
	syn Params =
	| DidChange {
		uri: String,
		version: Int,
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
			uri = uri,
			version = version,
			text = text
		}
	| "textDocument/didOpen" ->
		match mapLookup "textDocument" request.params with Some JsonObject textDocument in
		match mapLookup "uri" textDocument with Some JsonString uri in
		match mapLookup "version" textDocument with Some JsonInt version in
		match mapLookup "text" textDocument with Some JsonString text in

		DidChange {
			uri = uri,
			version = version,
			text = text
		}

	sem getDiagnostics uri version =
	| errors ->
		let error = head errors in
		match error with (info, msg) in
		let fileInfo = getFileInfo info in
		eprintln "[Compile Failed]";
	
		let uri = fileInfo.filename in
	
		getPublishDiagnostic uri version [
			jsonKeyObject [
				("message", JsonString msg),
				("severity", JsonInt 1),
				("source", JsonString "miking-lsp"),
				("range", jsonKeyObject [
					("start", jsonKeyObject [
						("line", JsonInt (subi fileInfo.lineStart 1)),
						("character", JsonInt fileInfo.colStart)
					]),
					("end", jsonKeyObject [
						("line", JsonInt (subi fileInfo.lineEnd 1)),
						("character", JsonInt fileInfo.colEnd)
					])
				])
			]
		]

	sem getEnvironment context uri = 
	| (expr, implementations) ->
		use MExprAst in
		use MExpr in

		-- let originalUri = uri in
		-- let strippedUri = "/mnt/ProbTime/examples/coin/coin.rpl" in
		let strippedUri = stripUriProtocol uri in

		let expr = symbolizeAllowFree expr in
		-- eprintln (join ["Symbolized: ", use MExprPrettyPrint in expr2str expr, "\n"]);

		recursive let createDefinitionLookup: Expr -> Map Name Info =
			lam expr.
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

		let definitionLookup = createDefinitionLookup expr in
		
		-- let seq = mapToSeq definitionLookup in
		-- let f = lam x. eprintln (join [nameGetStr x.0, ": ", info2str x.1]); () in
		-- eprintln "Definition Lookup:";
		-- iter f seq;

		recursive let createVariableLookup: Expr -> Map Info (Name, Type) = lam expr.
			let m = mapEmpty infoCmp in

			let m = switch expr
				case TmVar { ident=ident, ty=ty, info=info } then
					match info with Info realInfo then
						mapInsert info (ident, ty) m
					else
						m
				-- case expr & TmRecord { bindings=bindings, info=info } then
				-- 	-- eprintln "Record";
				-- 	-- eprintln (expr2str expr);
				-- 	m
				-- 	-- let seq = mapToSeq bindings in
				-- 	-- let f = lam x.
						
				-- 	-- 	()
				-- 	-- in
				-- 	-- iter f seq;
				-- 	-- m
				case _ then m
			end in

			sfold_Expr_Expr (lam acc. lam e.
				let children = createVariableLookup e in
				mapUnion acc children
			) m expr
		in

		let variableLookup = createVariableLookup expr in

		-- let seq = mapToSeq variableLookup in
		-- eprintln (join ["Variable Lookup (", int2string (length seq), "):"]);
		-- let f = lam x.
		-- 	let v = x.1 in
		-- 	eprintln (join [info2str x.0, ": ", nameGetStr v.0]); ()
		-- in
		-- iter f seq;

		recursive let findVariables = lam acc. lam variableLookupSeq. lam filename. lam line. lam character.
			match variableLookupSeq with [x] ++ seq then
				let info = x.0 in
				let variable = x.1 in
				let collision = infoCollision info filename line character in
				let acc = if collision then
					let name = variable.0 in
					let ty = variable.1 in
					concat [(info, name, ty)] acc
				else
					acc
				in

				findVariables acc seq filename line character
			else
				acc
		in

		let findVariable: String -> Int -> Int -> Option ((Info, Name, Type)) = lam filename. lam line. lam character.
			let foundVariables = findVariables [] (mapToSeq variableLookup) filename line character in
			match foundVariables with [x] ++ seq then
				let f = lam var1. lam var2.
					let info1 = var1.0 in
					let info2 = var2.0 in
					if infoContainsInfo info1 info2 then var1 else var2
				in
				Some (foldl f x seq)
			else
				None ()
		in

		recursive let _findDefinition = lam definitionLookupSeq. lam name.
			match definitionLookupSeq with [x] ++ seq then
				let info = x.1 in
				let collision = nameEq name x.0 in
				if collision then
					Some (info)
				else
					_findDefinition seq name
			else None ()
		in

		let findDefinition = _findDefinition (mapToSeq definitionLookup) in

		{
			context.environment with

			-- Definitions
			-- definitionLookup = definitionLookup,
			findDefinition = findDefinition,

			-- Variables
			-- variableLookup = variableLookup,
			findVariable = findVariable
		}

	sem execute context =
	| DidChange {uri = uri, version = version, text = text} ->
		-- let uri = "/mnt/ProbTime/examples/coin/coin.rpl" in

		switch context.compileFunc uri text
			case Left errors then
				eprintln "[Compile Error]";
				let response = getDiagnostics uri version errors in
				{
					response = Some(response),
					environment = context.environment
				}
			case Right file then
				eprintln "[Compile Success]";

				let response = getPublishDiagnostic uri version [] in
				let environment = getEnvironment context uri file in
				{
					response = Some(response),
					environment = environment
				}
		end

end