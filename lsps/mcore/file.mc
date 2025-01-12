include "mlang/main.mc"

include "../../lsp-server/lsp/root.mc"

type Path = String

lang MLangFileHandler = LanguageServer
  type Definitions = Map Name Info

  type Loaded = {
    content: String,
    filename: String
  }

  type ParseError = {
    loaded: Loaded,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }

  type Parsed = {
    loaded: Loaded,
    program: use MLangPipeline in MLangProgram,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }

  type Symbolized = {
    program: use MLangPipeline in MLangProgram,
    parsed: Parsed,
    symEnv: SymEnv,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }

  syn MLangFile =
  | CLoaded Loaded
  | CParseError ParseError
  | CParsed Parsed
  | CSymbolized Symbolized

  sem printFileKind: MLangFile -> String
  sem printFileKind =
  | CLoaded _ -> "Loaded"
  | CParseError _ -> "ParseError"
  | CParsed _ -> "Parsed"
  | CSymbolized _ -> "Symbolized"

  sem getFilename: MLangFile -> String
  sem getFilename =
  | CLoaded { filename = filename } -> filename
  | CParseError { loaded = loaded }
  | CParsed { loaded = loaded } -> getFilename (CLoaded loaded)
  | CSymbolized { parsed = parsed } -> getFilename (CParsed parsed)

  sem getContent: MLangFile -> String
  sem getContent =
  | CLoaded { content = content } -> content
  | CParseError { loaded = loaded }
  | CParsed { loaded = loaded } -> getContent (CLoaded loaded)
  | CSymbolized { parsed = parsed } -> getContent (CParsed parsed)

  sem getProgram: MLangFile -> Option MLangProgram
  sem getProgram =
  | CLoaded _ 
  | CParseError _ -> None ()
  | CParsed { program = program }
  | CSymbolized { program = program } -> Some program

  sem getIncludes: MLangFile -> [(Info, Include)]
  sem getIncludes =
  | CLoaded _
  | CParseError _ -> []
  | CParsed { includes = includes } -> includes
  | CSymbolized { parsed = parsed } -> getIncludes (CParsed parsed)

  sem getSymEnv: MLangFile -> Option SymEnv
  sem getSymEnv =
  | CLoaded _
  | CParseError _
  | CParsed _ -> None ()
  | CSymbolized { symEnv = symEnv } -> Some symEnv

  sem getFileErrors: MLangFile -> [Diagnostic]
  sem getFileErrors =
  | CLoaded _ -> []
  | CParseError { errors = errors } -> errors
  | CParsed { errors = errors } -> errors
  | CSymbolized { parsed = parsed, errors = errors } -> join [errors, getFileErrors (CParsed parsed)]

  sem getFileWarnings: MLangFile -> [Diagnostic]
  sem getFileWarnings =
  | CLoaded _ -> []
  | CParseError { warnings = warnings }
  | CParsed { warnings = warnings } -> warnings
  | CSymbolized { warnings = warnings, parsed = parsed } -> join [warnings, getFileWarnings (CParsed parsed)]
end