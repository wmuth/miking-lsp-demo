lang MLangParser = MLangFileHandler
  sem parseMLang : Path -> String -> MLangFile
  sem parseMLang path =| content ->
    eprintln (join ["Parsing: ", path]);

    let path = normalizeFilePath path in
    let dir = eraseFile path in
    -- let content = getContent (loader.load path).kind in

    let res = result.map (populateMLangProgramInfoWithFilename path) (use BootParserMLang in parseMLangString content) in
    let res = result.map (use MLangPipeline in constTransformProgram builtin) res in
    let res = mapErrors (errorWithFilename path) res in

    match result.consume res with (warnings, parsedResult) in
    switch parsedResult
      case Left errs then {
        kind = ParseError { content = content },
        errors = errs,
        warnings = warnings
      }
      case Right program then
        let includes = extractIncludes program.decls in
        match findPaths dir libs includes with (includeErrors, includeWarnings, includes) in
        let includes = map (lam v. (v.0, getIncludeFromPathStatus v.1)) includes in

        {
          kind = Parsed { content = content, program = program, includes = includes },
          errors = includeErrors,
          warnings = join [warnings, includeWarnings]
        }
    end
end