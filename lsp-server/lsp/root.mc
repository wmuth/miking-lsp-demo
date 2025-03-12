include "json.mc"
include "mexpr/ast.mc"

include "../../lib/utils.mc"
include "../json-rpc.mc"

include "./symbol-kind.mc"
include "./completion-kind.mc"

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

lang LanguageServerRoot =
  DiagnosticBase +
  LSPSymbolKind + LSPCompletionKind
  
  type LSPDefinition = {
    location: Info,
    kind: SymbolKind,
    documentation: () -> Option String,
    exported: Bool
  }

  type LSPCodeLens = {
    title: String,
    ideCommand: String,
    arguments: [JsonValue],
    data: Option JsonValue,
    location: Info
  }

  type LSPCompletion = {
    label: String,
    kind: CompletionItemKind,
    insertText: Option String,
    documentation: Option String,
    deprecated: Bool
  }

  type LanguageServerContext = {
    errors: [Diagnostic],
    warnings: [Diagnostic],
    information: [Diagnostic],

    lenses: [LSPCodeLens],

    hover: Map Info [() -> Option String],
    completions: Map Info [() -> LSPCompletion],

    definitions: Map Name [LSPDefinition],
    availability: Map Info [Name],
    usages: Map Info [Name],

    typeLocations: Map Info [Name],
    types: Map Name {
      location: Info,
      super: [Name] -- todo: iterate on this type since it can be primitive types as well
    }
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
  completions = mapEmpty infoCmp,
  definitions = mapEmpty nameSymCmp,
  availability = mapEmpty infoCmp,
  usages = mapEmpty infoCmp,
  typeLocations = mapEmpty infoCmp,
  types = mapEmpty nameSymCmp -- Map from a type to its super types
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

-- lang LanguageServerCompletion = LanguageServerRoot
--   type CompletionPayload = {
--     location: Option Info,
--     getCompletion: () -> Completion
--   }

--   syn LanguageServerPayload =
--   | LsCompletion CompletionPayload

--   sem populateContext context =
--   | LsCompletion (completion & { location=location, getCompletion=getCompletion }) ->
--     let location = optionGetOr (NoInfo ()) location in
--     { context with completions = mapInsertWith concat location [getCompletion] context.completions }
-- end

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
  -- | LsUsage _ -> context
  -- | LsUsage { location=location, name=name & !(_, _noSymbol) } ->
  | LsUsage { location=location, name=name } ->
    { context with usages = mapInsertWith concat location [name] context.usages } 
end

lang LanguageServerAvailability = LanguageServerRoot
  type AvailabilityPayload = {
    location: Info,
    name: Name
  }

  syn LanguageServerPayload =
  | LsAvailability AvailabilityPayload

  sem populateContext context =
  | LsAvailability { location=location, name=name } ->
    { context with availability = mapInsertWith concat location [name] context.usages } 
end

lang LanguageServerDefinition = LanguageServerRoot + LSPSymbolToCompletion
  type DefinitionPayload = {
    kind: SymbolKind,
    documentation: () -> Option String,
    exported: Bool,
    location: Info,
    name: Name
  }

  syn LanguageServerPayload =
  | LsDefinition DefinitionPayload

  sem populateContext context =
  | LsDefinition { location=location, name=name, kind=kind, documentation=documentation, exported=exported } ->
    {
      context with
      definitions = mapInsertWith concat name [{
        location=location,
        kind=kind,
        documentation=documentation,
        exported=exported
      }] context.definitions
    }
end

lang LanguageServerTypeHierarchy = LanguageServerRoot
  syn LanguageServerPayload =
  | LsType {
    location: Info,
    ident: Name,
    superIdents: [Name]
  }

  sem populateContext context =
  | LsType { ident=ident, superIdents=superIdents, location=location } ->
    {
      context with
      typeLocations = mapInsertWith concat location [ident] context.typeLocations,
      types = mapInsertWith (
        lam prev. lam new. {
          location = prev.location,
          super = concat prev.super new.super
        }
      ) ident {
        location = location,
        super = superIdents
      } context.types
    }
end

lang LanguageServerCodeLens = LanguageServerRoot
  syn LanguageServerPayload =
  | LsCodeLens LSPCodeLens

  sem populateContext context =
  | LsCodeLens lens ->
    { context with lenses = join [context.lenses, [lens]] }
end

lang LanguageServer =
  LanguageServerRoot +
  -- LanguageServerCompletion +
  LanguageServerHover +
  LanguageServerUsage +
  LanguageServerDefinition +
  LanguageServerTypeHierarchy +
  LanguageServerDiagnostic +
  LanguageServerCodeLens

  type LSPCompilationParameters = {
    uri: URI,
    content: String
  }

  type LSPCompilationResult = Map URI [LanguageServerPayload]

  type LSPConfig = {
    completion: Bool,
    hover: Bool,
    definition: Bool
  }

  type LSPOptions = {
    config: LSPConfig,
    serverName: String,
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
    files: Map URI LanguageServerContext,
    typeSymbols: Map Int Name -- Keep track of the symbol between type hierarchy requests
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
  serverName = "Miking LSP Server",
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