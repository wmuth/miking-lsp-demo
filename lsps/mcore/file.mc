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

  type TypeChecked = {
    expr: use MLangPipeline in Expr,
    symbolized: Symbolized,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }

  syn MLangFile =
  | CLoaded Loaded
  | CParseError ParseError
  | CParsed Parsed
  | CSymbolized Symbolized
  | CTypeChecked TypeChecked

  sem printFileKind: MLangFile -> String
  sem printFileKind =
  | CLoaded _ -> "Loaded"
  | CParseError _ -> "ParseError"
  | CParsed _ -> "Parsed"
  | CSymbolized _ -> "Symbolized"
  | CTypeChecked _ -> "TypeChecked"

  sem getFilename: MLangFile -> String
  sem getFilename =
  | CLoaded { filename = filename } -> filename
  | CParseError { loaded = loaded }
  | CParsed { loaded = loaded } -> getFilename (CLoaded loaded)
  | CSymbolized { parsed = parsed } -> getFilename (CParsed parsed)
  | CTypeChecked { symbolized = symbolized } -> getFilename (CSymbolized symbolized)

  sem getContent: MLangFile -> String
  sem getContent =
  | CLoaded { content = content } -> content
  | CParseError { loaded = loaded }
  | CParsed { loaded = loaded } -> getContent (CLoaded loaded)
  | CSymbolized { parsed = parsed } -> getContent (CParsed parsed)
  | CTypeChecked { symbolized = symbolized } -> getContent (CSymbolized symbolized)

  sem getProgram: MLangFile -> Option MLangProgram
  sem getProgram =
  | CLoaded _ 
  | CParseError _ -> None ()
  | CParsed { program = program }
  | CSymbolized { program = program } -> Some program
  | CTypeChecked { symbolized = symbolized } -> getProgram (CSymbolized symbolized)

  sem getSymEnv: MLangFile -> Option SymEnv
  sem getSymEnv =
  | CLoaded _
  | CParseError _
  | CParsed _ -> None ()
  | CSymbolized { symEnv = symEnv } -> Some symEnv
  | CTypeChecked { symbolized = symbolized } -> getSymEnv (CSymbolized symbolized)

  sem getFileErrors: MLangFile -> [Diagnostic]
  sem getFileErrors =
  | CLoaded _ -> []
  | CParseError { errors = errors } -> errors
  | CParsed { errors = errors } -> errors
  | CSymbolized { parsed = parsed, errors = errors } -> join [errors, getFileErrors (CParsed parsed)]
  | CTypeChecked { errors = errors, symbolized = symbolized } -> join [errors, getFileErrors (CSymbolized symbolized)]

  sem getFileWarnings: MLangFile -> [Diagnostic]
  sem getFileWarnings =
  | CLoaded _ -> []
  | CParseError { warnings = warnings }
  | CParsed { warnings = warnings } -> warnings
  | CSymbolized { warnings = warnings, parsed = parsed } -> join [warnings, getFileWarnings (CParsed parsed)]
  | CTypeChecked { warnings = warnings, symbolized = symbolized } -> join [warnings, getFileWarnings (CSymbolized symbolized)]
end