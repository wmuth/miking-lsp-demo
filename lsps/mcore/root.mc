include "mlang/main.mc"
include "../../lsp-server/lsp/root.mc"
include "./util.mc"

lang MLangBase = DiagnosticBase + MLangPipeline
  type Path = String
  type Link = (Info, Path)
end

lang MLangParsedRoot = MLangBase
  -- The idea of using diagnostic arrays instead of a 
  -- result type is to allow for recoverable errors.
  -- TODO: Investigate the possibility of using a
  -- type Diagnostic = (Info, String, Severity) to 
  -- allow for other types of diagnostics such as
  -- information and hints, while reducing the verbose
  -- "error" and "warning" fields.
  type MLangParsedFile = {
    program: Option MLangProgram,
    includes: [Link], -- Local paths, e.g. "foo.m"
    diagnostics: [DiagnosticWithSeverity]
  }
end

lang MLangLinkedRoot = MLangBase
  -- This file is constructed by looking at the
  -- includes from the parser, with the current
  -- directory of the file, in order to populate
  -- the includes with the full absolute path,
  -- and to report errors if the file does not exist.
  type MLangLinkedFile = {
    program: Option MLangProgram, -- With DeclInclude removed
    links: [Link], -- Absolute paths, e.g. "/home/user/foo.m"
    diagnostics: [DiagnosticWithSeverity]
  }
end

lang MLangSymbolizedRoot = MLangBase
  type MLangSymbolizedFile = {
    program: Option MLangProgram, -- With symbols
    symEnv: SymEnv, -- symEnvEmpty
    diagnostics: [DiagnosticWithSeverity]
  }
end

lang MLangTypeCheckedRoot = MLangBase
  type MLangTypeCheckedFile = {
    expr: Option Expr, -- With symbols
    tcEnv: TCEnv,
    compositionEnv: CompositionCheckEnv,
    diagnostics: [DiagnosticWithSeverity]
  }
end

lang MLangRoot =
  LanguageServer + MLangBase +
  MLangParsedRoot + MLangLinkedRoot + MLangSymbolizedRoot + MLangTypeCheckedRoot

  -- The status of a file represents not what the file is
  -- currently providing (e.g. Symbolized does not mean that
  -- the file is necessarily providing a symEnv, or a symbolized
  -- program), but rather to what extent we have processed the
  -- file.
  -- 
  -- That means, a status of Symbolized means that either the
  -- file has been symbolized, or that the symbolization has failed
  -- and we have diagnostic information about it.
  -- 
  -- The idea of this system is to allow for making files "dirty",
  -- when dependencies change (i.e. includes), or "changed" when
  -- the in-memory contents of the file change.
  syn MLangFileStatus =
  | Changed -- File content has changed, needs re-parsing
  | Parsed
  | Dirty -- Dependency has changed, needs re-symbolization
  | Linked
  | Symbolized
  | TypeChecked

  sem mLangFileStatusToString : MLangFileStatus -> String
  sem mLangFileStatusToString =
  | Changed () -> "Changed"
  | Parsed () -> "Parsed"
  | Dirty () -> "Dirty"
  | Linked () -> "Linked"
  | Symbolized () -> "Symbolized"
  | TypeChecked () -> "TypeChecked"

  type MLangFile = {
    status: MLangFileStatus,
    filename: Path,
    content: String,

    parsed: Option MLangParsedFile,
    linked: Option MLangLinkedFile,
    symbolized: Option MLangSymbolizedFile,
    typeChecked: Option MLangTypeCheckedFile
  }

  sem getProgram : MLangFile -> Option MLangProgram
  sem getProgram =
  | _ -> None ()
  | { status = Parsed (), parsed = Some { program = program } }
  | { status = Linked (), linked = Some { program = program } }
  | { status = Symbolized () | TypeChecked (), symbolized = Some { program = program } } ->
    program

  sem _getDiagnostics : MLangFile -> MLangFileStatus -> [DiagnosticWithSeverity]
  sem _getDiagnostics file =
  | _
  | Changed () -> []
  | Parsed ()
  | Dirty () -> join [
    optionMapOr [] (lam v. v.diagnostics) file.parsed
  ]
  | Linked () -> join [
    _getDiagnostics file (Parsed ()),
    optionMapOr [] (lam v. v.diagnostics) file.linked
  ]
  | Symbolized () -> join [
    _getDiagnostics file (Linked ()),
    optionMapOr [] (lam v. v.diagnostics) file.symbolized
  ]
  | TypeChecked () -> join [
    _getDiagnostics file (Symbolized ()),
    optionMapOr [] (lam v. v.diagnostics) file.typeChecked
  ]

  sem getSymEnv : MLangFile -> Option SymEnv
  sem getSymEnv =
  | _ -> None ()
  | { status=Symbolized () | TypeChecked (), symbolized=Some symbolized } -> Some symbolized.symEnv

  sem getFileDiagnostics : MLangFile -> [DiagnosticWithSeverity]
  sem getFileDiagnostics =| file ->
    _getDiagnostics file file.status

  sem createEmptyFile : Path -> String -> MLangFile
  sem createEmptyFile filename =| content ->
    {
      status = Changed (),
      filename = filename,
      content = content,
      parsed = None (),
      linked = None (),
      symbolized = None (),
      typeChecked = None ()
    }
end