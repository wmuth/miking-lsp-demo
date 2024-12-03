include "json.mc"
include "mexpr/ast.mc"

include "../../lib/utils.mc"
include "../json-rpc.mc"

type CompilationResult = {
  expr: use MExprAst in Option Expr, -- Todo: this is Mexpr specific, should be abstracted
  errors: [(Info, String)],
  warnings: [(Info, String)]
}

type CompilationParameters = {
  uri: String,
  content: String,

  -- When we know e.g. that the result is error-free,
  -- we can notify the server about the partial result.
  notifyPartialResult: CompilationResult -> ()
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
  sendNotification: JsonValue -> (),
  environment: LSPEnvironment
}

type LSPResult = {
  response: Option JsonValue,
  environment: LSPEnvironment
}

lang LSPRoot
  syn Message =

  -- Translate RPC message to LSP Params object, to be used in `execute`
  sem getMessage: RPCRequest -> String -> Message

  -- todo: return abstracted LSP response
  sem execute: LSPExecutionContext -> Message -> LSPResult
end
