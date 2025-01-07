include "./include-handler.mc"

lang MLangFileHandler = MLangIncludeHandler
  type Definitions = Map Name Info

  type Loaded = {
    content: String
  }

  type ParseError = {
    loaded: Loaded,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }

  type Parsed = {
    loaded: Loaded,
    program: use MLangIncludeHandler in MLangProgram,
    includes: [(Info, use MLangIncludeHandler in Include)],
    includeErrors: [Diagnostic],
    warnings: [Diagnostic]
  }

  -- We have loaded the included files
  type Linked = {
    parsed: Parsed,
    program: use MLangIncludeHandler in MLangProgram, -- Without includes
    links: [(Info, Path, MLangFile)],
    linkErrors: [Diagnostic],
    warnings: [Diagnostic]
  }

  type Symbolized = {
    program: use MLangIncludeHandler in MLangProgram,
    linked: Linked,
    definitions: Definitions,
    symEnv: SymEnv,
    warnings: [Diagnostic],
    errors: [Diagnostic]
  }

  syn MLangFile =
  | CLoaded Loaded
  | CParseError ParseError
  | CParsed Parsed
  | CLinked Linked
  | CSymbolized Symbolized

  sem printFileKind: MLangFile -> String
  sem printFileKind =
  | CLoaded _ -> "Loaded"
  | CParseError _ -> "ParseError"
  | CParsed _ -> "Parsed"
  | CLinked _ -> "Linked"
  | CSymbolized _ -> "Symbolized"

  sem getContent: MLangFile -> String
  sem getContent =
  | CLoaded { content = content } -> content
  | CParseError { loaded = loaded }
  | CParsed { loaded = loaded } -> getContent (CLoaded loaded)
  | CLinked { parsed = parsed } -> getContent (CParsed parsed)
  | CSymbolized { linked = linked } -> getContent (CLinked linked)

  sem getProgram: MLangFile -> Option MLangProgram
  sem getProgram =
  | CLoaded _ 
  | CParseError _ -> None ()
  | CLinked { program = program }
  | CParsed { program = program }
  | CSymbolized { program = program } -> Some program

  sem getIncludes: MLangFile -> [(Info, Include)]
  sem getIncludes =
  | CLoaded _
  | CParseError _ -> []
  | CParsed { includes = includes } -> includes
  | CLinked { parsed = parsed } -> getIncludes (CParsed parsed)
  | CSymbolized { linked = linked } -> getIncludes (CLinked linked)

  sem getSymEnv: MLangFile -> Option SymEnv
  sem getSymEnv =
  | CLoaded _
  | CParseError _
  | CParsed _
  | CLinked _ -> None ()
  | CSymbolized { symEnv = symEnv } -> Some symEnv

  sem getDefinitions: MLangFile -> Definitions
  sem getDefinitions =
  | CLoaded _
  | CParseError _
  | CParsed _
  | CLinked _ -> mapEmpty nameCmp
  | CSymbolized { definitions = definitions } -> definitions

  sem getFileErrors: MLangFile -> [Diagnostic]
  sem getFileErrors =
  | CLoaded _ -> []
  | CParseError { errors = errors } -> errors
  | CParsed { includeErrors = includeErrors } -> includeErrors
  | CLinked { linkErrors = linkErrors, parsed = parsed } -> join [linkErrors, getFileErrors (CParsed parsed)]
  | CSymbolized { linked = linked, errors = errors } -> join [errors, getFileErrors (CLinked linked)]

  sem getFileWarnings: MLangFile -> [Diagnostic]
  sem getFileWarnings =
  | CLoaded _ -> []
  | CParseError { warnings = warnings }
  | CParsed { warnings = warnings } -> warnings
  | CLinked { warnings = warnings, parsed = parsed } -> join [warnings, getFileWarnings (CParsed parsed)]
  | CSymbolized { warnings = warnings, linked = linked } -> join [warnings, getFileWarnings (CLinked linked)]

  sem getIncludePaths: MLangFile -> [(Info, Path)]
  sem getIncludePaths =| file ->
    let includes = getIncludes file in
    let f = lam inc. optionMap (lam v. (inc.0, v)) (inc2str inc.1) in
    let paths = map f includes in
    filterOption paths

  sem getIncludePathStrings: MLangFile -> [Path]
  sem getIncludePathStrings =| file ->
    map (lam v. v.1) (getIncludePaths file)
end

let mLangFile2string: MLangFile -> String = lam file.
  use MLangFileHandler in
  join [
    "MLangFile: ", printFileKind file, " {\n",
    "  errors = ", strJoin "\n\t" (map diagnostic2string (getFileErrors file)), "\n",
    "  warnings = ", strJoin "\n\t" (map diagnostic2string (getFileWarnings file)), "\n",
    "}"
  ]

mexpr

let emptyInfo = makeInfo {filename = "", row = 0, col = 0} {filename = "", row = 0, col = 0} in

use MLangFileHandler in

let loadedFile = CLoaded { content = "content" } in
utest getIncludes loadedFile with [] in

let parsedFile = CParsed { loaded = loadedFile, parsed = { decls = [], expr = uunit_ }, includes = [
  (emptyInfo, ExistingFile "path/./"), (emptyInfo, NonExistentFiles ["abc"])
] } in
utest getIncludes parsedFile with ["path"] in

()