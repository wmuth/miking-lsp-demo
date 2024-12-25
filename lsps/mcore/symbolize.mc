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
  | file & { kind = Parsed { program = program, includes = includes, content = content } } ->
    eprintln (join ["Symbolizing: ", path]);

    let symEnvs = filterMap (lam file. getSymEnv file.kind) fileIncludes in
    let symEnvs = foldl mergeSymEnv symEnvDefault symEnvs in

    {
      file with
      kind = Symbolized {
        content = content,
        program = program,
        includes = includes,
        symEnv = symEnvDefault
      }
    }
end