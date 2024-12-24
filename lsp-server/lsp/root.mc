include "json.mc"
include "mexpr/ast.mc"

include "../../lib/utils.mc"
include "../json-rpc.mc"

type URI = String
type Diagnostic = (Info, String)

type CodeLens = {
  title: String,
  ideCommand: String,
  commands: [JsonValue],
  data: JsonValue,
  location: Info
}

type LookupResult = {
  info: Info,
  pprint: () -> Option String,
  lookupDefinition: Option (() -> Info)
}

type CompilationResult = {
  errors: [(Info, String)],
  warnings: [(Info, String)],
  lenses: [CodeLens],
  lookup: Int -> Int -> Option LookupResult
}

type CompilationParameters = {
  uri: URI,
  content: String
  -- -- URI, Result
  -- update: String -> CompilationResult -> ()
}

type LSPOptions = {
  completion: Bool,
  hover: Bool,
  definition: Bool,
  pruneMessages: Bool
}

let defaultLSPOptions: LSPOptions = {
  completion = true,
  hover = true,
  definition = true,
  pruneMessages = true
}

type LSPStartParameters = {
  onOpen: CompilationParameters -> Map URI CompilationResult,
  onChange: CompilationParameters -> Map URI CompilationResult,
  onClose: String -> (),
  options: LSPOptions
}

type LSPFileEnvironment = {
  lookup: Int -> Int -> Option LookupResult

  -- findVariable: use MExprAst in String -> Int -> Int -> Option ((Info, Name, Type)), -- Todo: this is Mexpr specific, should be abstracted
  -- findVariable: String -> Int -> Int -> (() -> Option VariableLookupResult),
  -- findDefinition: Name -> Option (Info), -- Todo: this is Mexpr specific, should be abstracted

  -- TODO: Temporary in order to support naive completion
  -- definitionLookup: Map Name Info,
  -- utestLookup: [Info]
}

type LSPEnvironment = {
  files: Map URI LSPFileEnvironment
}

type LSPExecutionContext = {
  parameters: LSPStartParameters,
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
  sem execute context =
  | message -> {
    response = None (),
    environment = context.environment
  }
end