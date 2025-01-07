lang MLangSymbolize = MLangFileHandler
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
    eprintln (join ["Symbolizing: ", path]);

    let symEnvs = filterMap (lam file. getSymEnv file) fileIncludes in
    let symEnvs = foldl mergeSymEnv symEnvDefault symEnvs in

    CSymbolized {
      linked = linked,
      symEnv = symEnvDefault,
      warnings = [] -- todo
    }
end