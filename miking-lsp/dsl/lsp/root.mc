include "json.mc"
include "mexpr/ast.mc"

include "./definitions.mc"
include "../utils.mc"
include "../json-rpc.mc"

type CompileFunc = use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations)

type LSPExecutionContext = {
	compileFunc: CompileFunc
}

lang LSPRoot
	syn Params =
	sem getParams: RPCRequest -> String -> Params
	sem execute: LSPExecutionContext -> Params -> Option JsonValue -- todo: return abstracted LSP response
end