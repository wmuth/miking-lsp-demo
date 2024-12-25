include "./include-handler.mc"

type ParsedMLangFile = use MLangIncludeHandler in {
  content: String,
  program: MLangProgram,
  includes: [(Info, Include)]
}

type SymbolizedMLangFile = use MLangIncludeHandler in {
  content: String,
  program: MLangProgram,
  includes: [(Info, Include)],
  symEnv: SymEnv
}

lang MLangFileHandler = MLangIncludeHandler
  syn MLangFileKind =
  | Loaded { content: String }
  | ParseError { content: String }
  | Parsed ParsedMLangFile
  | Symbolized SymbolizedMLangFile

  sem getContent: MLangFileKind -> String
  sem getContent =
  | Loaded { content = content }
  | ParseError { content = content }
  | Parsed { content = content } -> content
  | Symbolized { content = content } -> content

  sem getParsed: MLangFileKind -> Option MLangProgram
  sem getParsed =
  | Loaded _ 
  | ParseError _ -> None ()
  | Parsed { program = program }
  | Symbolized { program = program } -> Some program

  sem getIncludes: MLangFileKind -> [(Info, Include)]
  sem getIncludes =
  | Loaded _
  | ParseError _ -> []
  | Parsed { includes = includes }
  | Symbolized { includes = includes } -> includes

  sem getSymEnv: MLangFileKind -> Option SymEnv
  sem getSymEnv =
  | Loaded _
  | ParseError _
  | Parsed _ -> None ()
  | Symbolized { symEnv = symEnv } -> Some symEnv

  sem getIncludePaths: MLangFileKind -> [(Info, Path)]
  sem getIncludePaths =| kind ->
    let includes = getIncludes kind in
    let f = lam inc. optionMap (lam v. (inc.0, v)) (inc2str inc.1) in
    let paths = map f includes in
    filterOption paths
end

type MLangFile = {
  kind: use MLangFileHandler in MLangFileKind,
  errors: [Diagnostic],
  warnings: [Diagnostic]
}