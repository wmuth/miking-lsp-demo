include "json.mc"
include "mexpr/ast.mc"

include "../../lib/utils.mc"
include "../json-rpc.mc"

type URI = String
type Diagnostic = (Info, String)

type CodeLens = {
  title: String,
  ideCommand: String,
  arguments: [JsonValue],
  data: JsonValue,
  location: Info
}

lang LanguageServerRoot
  type LanguageServerContext = {
    errors: [Diagnostic],
    warnings: [Diagnostic],
    information: [Diagnostic],

    lenses: [CodeLens],

    hover: Map Info [() -> Option String],
    definitions: Map Name [Info],
    usages: Map Info [Name]
  }

  syn LanguageServerPayload =

  sem populateContext: LanguageServerContext -> LanguageServerPayload -> LanguageServerContext
  sem populateContext context =| _ -> eprintln "Undefined populateContext"; context
end

let emptyLanguageServerContext = {
  errors = [],
  warnings = [],
  information = [],
  lenses = [],
  hover = mapEmpty infoCmp,
  definitions = mapEmpty nameCmp,
  usages = mapEmpty infoCmp
}

lang LanguageServerDiagnostic = LanguageServerRoot
  syn DiagnosticSeverity =
  | Error
  | Warning
  | Information

  type DiagnosticWithSeverity = {
    location: Info,
    severity: DiagnosticSeverity,
    message: String
  }

  sem clearSeverity: DiagnosticWithSeverity -> Diagnostic
  sem clearSeverity =| diagnostic -> (diagnostic.location, diagnostic.message)

  syn LanguageServerPayload =
  | LsDiagnostic DiagnosticWithSeverity

  sem populateContext context =
  | LsDiagnostic (diagnostic & { severity=Error () }) ->
    { context with errors = join [context.errors, [clearSeverity diagnostic]] }
  | LsDiagnostic (diagnostic & { severity=Warning () }) ->
    { context with warnings = join [context.warnings, [clearSeverity diagnostic]] }
  | LsDiagnostic (diagnostic & { severity=Information () }) ->
    { context with information = join [context.information, [clearSeverity diagnostic]] }
end

lang LanguageServerHover = LanguageServerRoot
  type HoverPayload = {
    location: Info,
    toString: () -> Option String
  }

  syn LanguageServerPayload =
  | LsHover HoverPayload

  sem populateContext context =
  | LsHover (hover & { location=location, toString=toString }) ->
    { context with hover = mapInsertWith concat location [toString] context.hover }
end

lang LanguageServerUsage = LanguageServerRoot
  type UsagePayload = {
    location: Info,
    name: Name
  }

  syn LanguageServerPayload =
  | LsUsage UsagePayload

  sem populateContext context =
  | LsUsage { location=location, name=name } ->
    { context with usages = mapInsertWith concat location [name] context.usages }
    
end

lang LanguageServerDefinition = LanguageServerRoot
  type DefinitionPayload = {
    location: Info,
    name: Name
  }

  syn LanguageServerPayload =
  | LsDefinition DefinitionPayload

  sem populateContext context =
  | LsDefinition { location=location, name=name } ->
    { context with definitions = mapInsertWith concat name [location] context.definitions }
end

lang LanguageServerCodeLens = LanguageServerRoot
  type CodeLens = {
    title: String,
    ideCommand: String,
    arguments: [JsonValue],
    data: JsonValue,
    location: Info
  }

  syn LanguageServerPayload =
  | LsCodeLens CodeLens
end

lang LanguageServer =
  LanguageServerRoot +
  LanguageServerHover +
  LanguageServerUsage +
  LanguageServerDefinition +
  LanguageServerDiagnostic +
  LanguageServerCodeLens

  -- type CompilationDiagnostics = {
  --   errors: [Diagnostic],
  --   warnings: [Diagnostic]
  -- }

  type CompilationParameters = {
    uri: URI,
    content: String
  }

  type LSPConfig = {
    completion: Bool,
    hover: Bool,
    definition: Bool
  }

  type LSPOptions = {
    config: LSPConfig,
    pruneMessages: Bool
  }

  type LSPStartParameters = {
    onOpen: CompilationParameters -> Map URI [LanguageServerPayload],
    onChange: CompilationParameters -> Map URI [LanguageServerPayload],
    onClose: String -> (),
    options: LSPOptions
  }

  type LSPEnvironment = {
    files: Map URI LanguageServerContext
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
end

let emptyCompilationResult = {
  errors = [],
  warnings = [],
  lenses = [],
  lookup = lam. lam. None ()
  -- dirtiedFiles = setEmpty cmpString
}

let defaultLSPOptions: use LanguageServer in LSPOptions = {
  config = {
    completion = true,
    hover = true,
    definition = true
  },
  pruneMessages = true
}

lang LSPRoot = LanguageServer
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