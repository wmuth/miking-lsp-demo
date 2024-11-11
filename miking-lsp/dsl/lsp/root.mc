include "json.mc"
include "mexpr/ast.mc"

include "./definitions.mc"
include "../utils.mc"
include "../json-rpc.mc"

type CompileFunc = use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations)

type LSPEnvironment = {
	findVariable: String -> Int -> Int -> Option ((Info, Name, use MExprAst in Type)),
	findDefinition: Name -> Option (Info)
}

type LSPExecutionContext = {
	compileFunc: CompileFunc,
	environment: LSPEnvironment
}

type LSPResult = {
	response: Option JsonValue,
	environment: LSPEnvironment
}

lang LSPRoot
	syn Params =

	-- Translate RPC message to LSP Params object, to be used in `execute`
	sem getParams: RPCRequest -> String -> Params

	 -- todo: return abstracted LSP response
	sem execute: LSPExecutionContext -> Params -> LSPResult
end