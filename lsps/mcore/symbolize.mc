include "main.mc"

lang SymbolizeMLangLSP = MLangPipeline
  type SymbolizedMLangLSP = {
    program: MLangProgram,
    symEnv: SymEnv,
    warnings: [Diagnostic],
    errors: [Diagnostic]
  }

  sem symbolizeMLangLSP : SymEnv -> MLangProgram -> SymbolizedMLangLSP
  sem symbolizeMLangLSP symEnv =| program ->
    -- Ugly hacking to not make symbolizeExpr
    -- crash in the MLang pipeline
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    match use MLangPipeline in symbolizeMLang symEnv program with (symEnv, program) in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      program = program,
      symEnv = symEnv,
      warnings = warnings,
      errors = errors
    }
end

lang MLangSymbolize
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

  sem mergeSymEnv : SymEnv -> SymEnv -> SymEnv
  sem mergeSymEnv a =| b -> {
    allowFree = b.allowFree,
    ignoreExternals = b.ignoreExternals,
    currentEnv = mergeNameEnv a.currentEnv b.currentEnv,
    langEnv = mapUnionWith mergeNameEnv a.langEnv b.langEnv,
    namespaceEnv = mapUnion a.namespaceEnv b.namespaceEnv
  }

	sem symbolizeMLangLanguageSupport : Path -> MLangFile -> MLangFile
	sem symbolizeMLangLanguageSupport path =
  | file & CParsed parsed ->
    let symEnvDefault = {
      symEnvDefault with
      allowFree = true
    } in

    match use SymbolizeMLangLSP in symbolizeMLangLSP symEnvDefault parsed.program
    with { program = program, symEnv = symEnv, warnings = warnings, errors = errors } in

    CSymbolized {
      program = program,
      parsed = parsed,
      symEnv = symEnv,
      warnings = warnings,
      errors = errors
    }
end