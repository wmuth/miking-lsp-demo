include "json.mc"
include "mexpr/ast.mc"

include "../../lib/utils.mc"
include "../json-rpc.mc"

type URI = String
type Diagnostic = (Info, String)

lang DiagnosticBase
  syn Severity =
  | Error
  | Warning
  | Information

  type DiagnosticWithSeverity = (Info, String, Severity)

  sem clearSeverity : DiagnosticWithSeverity -> Diagnostic
  sem clearSeverity =| (info, message, _) -> (info, message)

  sem addSeverity : Severity -> Diagnostic -> DiagnosticWithSeverity
  sem addSeverity severity =| (info, message) -> (info, message, severity)
end

lang LanguageServerRoot = DiagnosticBase
  type CodeLens = {
    title: String,
    ideCommand: String,
    arguments: [JsonValue],
    data: Option JsonValue,
    location: Info
  }

  type LanguageServerContext = {
    errors: [Diagnostic],
    warnings: [Diagnostic],
    information: [Diagnostic],

    lenses: [CodeLens],

    hover: Map Info [() -> Option String],
    definitions: Map Name [Info],
    usages: Map Info [Name],
    gotos: Map Info [Info]
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
  definitions = mapEmpty nameSymCmp,
  usages = mapEmpty infoCmp,
  gotos = mapEmpty infoCmp
}

lang LanguageServerDiagnostic = LanguageServerRoot
  syn LanguageServerPayload =
  | LsDiagnostic DiagnosticWithSeverity

  sem populateContext context =
  | LsDiagnostic (diagnostic & (_, _, Error ())) ->
    { context with errors = join [context.errors, [clearSeverity diagnostic]] }
    | LsDiagnostic (diagnostic & (_, _, Warning ())) ->
    { context with warnings = join [context.warnings, [clearSeverity diagnostic]] }
    | LsDiagnostic (diagnostic & (_, _, Information ())) ->
    { context with information = join [context.information, [clearSeverity diagnostic]] }
end

lang LanguageServerGoto = LanguageServerRoot
  syn LanguageServerPayload =
  | LsGoto {
    from: Info,
    to: Info
  }

  sem populateContext context =
  | LsGoto { from=from, to=to } ->
    { context with gotos = mapInsertWith concat from [to] context.gotos }
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
  syn LanguageServerPayload =
  | LsCodeLens CodeLens

  sem populateContext context =
  | LsCodeLens lens ->
    { context with lenses = join [context.lenses, [lens]] }
end

lang LanguageServer =
  LanguageServerRoot +
  LanguageServerGoto +
  LanguageServerHover +
  LanguageServerUsage +
  LanguageServerDefinition +
  LanguageServerDiagnostic +
  LanguageServerCodeLens

  syn EventType =
  | Open
  | Change

  type LSPCompilationParameters = {
    uri: URI,
    content: String,
    typ: EventType
  }

  type LSPCompilationResult = Map URI [LanguageServerPayload]

  type LSPConfig = {
    completion: Bool,
    hover: Bool,
    definition: Bool
  }

  type LSPOptions = {
    config: LSPConfig,
    extension: String,
    indexWorkspace: Bool,
    printClientMessages: Bool,
    pruneMessages: Bool
  }

  type LSPStartParameters = {
    onOpen: LSPCompilationParameters -> LSPCompilationResult,
    onChange: LSPCompilationParameters -> LSPCompilationResult,
    onClose: String -> (),
    options: LSPOptions
  }

  type LSPEnvironment = {
    rootUri: Option URI,
    options: LSPOptions,
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

let defaultLSPOptions: use LanguageServer in LSPOptions = {
  config = {
    completion = true,
    hover = true,
    definition = true
  },
  extension = "",
  indexWorkspace = true,
  printClientMessages = false,
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