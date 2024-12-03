include "json.mc"
include "mexpr/ast.mc"

include "./definitions.mc"
include "../../lib/utils.mc"
include "../json-rpc.mc"

type CompilationResult = {
  expr: use MExprAst in Option Expr, -- Todo: this is Mexpr specific, should be abstracted
  errors: [(Info, String)],
  warnings: [(Info, String)]
}

type CompilationParameters = {
  uri: String,
  content: String
}

type CompileFunc = CompilationParameters -> CompilationResult

type LSPFileEnvironment = {
	findVariable: use MExprAst in String -> Int -> Int -> Option ((Info, Name, Type)), -- Todo: this is Mexpr specific, should be abstracted
	findDefinition: Name -> Option (Info), -- Todo: this is Mexpr specific, should be abstracted

	errors: [(Info, String)],
  warnings: [(Info, String)]
}

type LSPEnvironment = {
	files: Map String LSPFileEnvironment
}

type LSPExecutionContext = {
	compileFunc: CompileFunc,
	environment: LSPEnvironment
}

type LSPResult = {
	response: Option [JsonValue],
	environment: LSPEnvironment
}

lang LSPRoot
	syn Params =

	-- Translate RPC message to LSP Params object, to be used in `execute`
	sem getParams: RPCRequest -> String -> Params

	 -- todo: return abstracted LSP response
	sem execute: LSPExecutionContext -> Params -> LSPResult
end
