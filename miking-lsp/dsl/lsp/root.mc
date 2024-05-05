include "json.mc"

include "../utils.mc"

-- type ParseFunc = String -> String -> Either [(Info, String)] [(Info, String)]
type ParseFunc

type LSPExecutionContext = {
	-- Currently does not work, as the parse function is dynamically generated
	-- parseFunc: ParseFunc
}

lang LSPRoot
	syn Params =
	sem getParams: RPCRequest -> String -> Params
	sem execute: LSPExecutionContext -> Params -> Option JsonValue -- todo: return abstracted LSP response
end