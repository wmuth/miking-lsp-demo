lang MLangSymbolize = MLangFileHandler
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

	sem symbolizeMLang : Path -> [MLangFile] -> MLangFile -> MLangFile
	sem symbolizeMLang path fileIncludes =
  | file & CLinked linked ->
    let symEnvs = filterMap (lam file. getSymEnv file) fileIncludes in
    let symEnv = foldl mergeSymEnv symEnvDefault symEnvs in

    match use MLangPipeline in symbolizeMLang symEnv (linked.program) with (symEnv, program) in

    -- eprintln (join ["Symbolized ", path]);
    -- eprintln (env2str symEnv);
    -- eprintln "Done symbolizing MLang file.";

    CSymbolized {
      program = program,
      linked = linked,
      symEnv = symEnv,
      warnings = [] -- todo
    }
end