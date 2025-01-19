include "./util.mc"
include "./root.mc"
include "./linker.mc"

lang MLangSymbolizer = MLangRoot + MLangLinker
  type IncludeResult = {
    files: [MLangFile],
    diagnostics: [DiagnosticWithSeverity]
  }

  sem getIncludedFiles : (Path -> Option MLangFile) -> [Link] -> IncludeResult
  sem getIncludedFiles getFile =| includes ->
    let files = map (
      lam inc.
        match inc with (info, path) in
        let file = getFile path in
        let file = optionMap (lam file. result.ok file) file in
        let file = optionGetOrElse (lam. result.err (info, join ["Include not found (", path, ")"])) file in
        let file = result.bind file (
          lam file.
            if geqi (length (getFileDiagnostics file)) 1 then
              result.withAnnotations (result.warn (info, join ["Errors or warnings in included file (", path, ")"])) (result.ok file)
            else
              result.ok file
        ) in
        file
    ) includes in

    let results = map result.consume files in
    let warnings = flatMap fst results in
    let results = map snd results in
    let errors = flatten (eitherLefts results) in
    let files = eitherRights results in

    let linkerResult: IncludeResult = {
      files = files,
      diagnostics = join [
        map (addSeverity (Error ())) errors,
        map (addSeverity (Warning ())) warnings
      ]
    } in

    linkerResult

  sem mergeSymEnv : SymEnv -> SymEnv -> SymEnv
  sem mergeSymEnv a =| b -> {
    allowFree = b.allowFree,
    ignoreExternals = b.ignoreExternals,
    currentEnv = mergeNameEnv a.currentEnv b.currentEnv,
    langEnv = mapUnionWith mergeNameEnv a.langEnv b.langEnv,
    namespaceEnv = mapUnion a.namespaceEnv b.namespaceEnv
  }

  sem getSymEnv : SymEnv -> MLangFile -> SymEnv
  sem getSymEnv default =
  | { status=Symbolized (), symbolized=Some symbolized } -> symbolized.symEnv
  | _ -> default

  sem env2str : SymEnv -> String
  sem env2str =| env ->
    let f = lam value.
      match value with (key, value) in
      join [key, ": ", nameGetStr value]
    in

    let envs = mapFromSeq cmpString [
      ("varEnv", env.currentEnv.varEnv),
      ("conEnv", env.currentEnv.conEnv),
      ("tyEnv", env.currentEnv.tyVarEnv),
      ("tyConEnv", env.currentEnv.tyConEnv),
      ("reprEnv", env.currentEnv.reprEnv)
    ] in

    strJoin "\n" (
      map
      (lam x. join [x.0, ": \n\t", strJoin "\n\t" (map f (mapToSeq x.1))])
      (mapToSeq envs)
    )

  sem lsSymbolizeMLang : (Path -> Option MLangFile) -> Path -> [Link] -> Option MLangProgram -> MLangSymbolizedFile
  sem lsSymbolizeMLang getFile filename includes =| program ->
    let symEnvEmpty = {
      symEnvEmpty with
      allowFree = true
    } in

    let linkerResult = getIncludedFiles getFile includes in
    let symEnvs = map (getSymEnv symEnvEmpty) linkerResult.files in
    let symEnv = foldl mergeSymEnv symEnvEmpty symEnvs in

    -- Ugly hacking to not make symbolizeExpr
    -- crash in the MLang pipeline
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    let res = optionMap (symbolizeMLang symEnv) program in
    let res = optionMap (lam res. match res with (symEnv, program) in (symEnv, Some program)) res in
    match optionGetOr (symEnvEmpty, None ()) res with (symEnv, program) in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      program = program,
      symEnv = symEnv,
      diagnostics = join [
        linkerResult.diagnostics,
        map (addSeverity (Error ())) errors,
        map (addSeverity (Warning ())) warnings
      ]
    }
end