type LocalPath = String
type Path = String

lang MLangAndMExpr = MLangAst + MExprAst
end

lang MLangCompilationKind = MLangAndMExpr
  -- We store not found included files,
  -- so that when a non-existing file is created,
  -- we can update the diagnostics of all dependents (dirty paths handling).
  syn Include =
  | ExistingFile Path
  | NotFound [Path]

  sem inc2str: Include -> Option String
  sem inc2str =
  | ExistingFile path -> Some (normalizeFilePath path)
  | NotFound paths -> None ()

  syn MLangFileKind =
  | Loaded { content: String }
  | Parsed { content: String, parsed: MLangProgram, includes: [(Info, Include)] }
end