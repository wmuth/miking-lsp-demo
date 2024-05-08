include "json.mc"
include "mexpr/ast.mc"

include "../utils.mc"

-- type ParseFunc = String -> String -> Either [(Info, String)] [(Info, String)]
-- type CompileFunc = String -> Expr

type LSPExecutionContext = {
	-- compileFunc: CompileFunc
}

lang LSPRoot
	syn Params =
	sem getParams: RPCRequest -> String -> Params
	sem execute: LSPExecutionContext -> Params -> Option JsonValue -- todo: return abstracted LSP response
end