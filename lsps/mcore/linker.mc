include "./util.mc"
include "./root.mc"
include "./include-handler.mc"

lang MLangLinker = MLangRoot + MLangPathHandler
  sem clearDeclIncludes : [Decl] -> [Decl]
  sem clearDeclIncludes =
  | [] -> []
  | [DeclInclude _] ++ rest -> clearDeclIncludes rest
  | [decl] ++ rest -> concat [decl] (clearDeclIncludes rest)

  sem lsLinkMLang : Path -> [Link] -> Option MLangProgram -> MLangLinkedFile
  sem lsLinkMLang filename includes =| program ->
    let currentPath = normalizeFilePath filename in
    let dir = eraseFile currentPath in

    let includedFiles: [Result Diagnostic Diagnostic Link] = map (
      lam inc.
        match inc with (info, path) in
        let path = findLocalPath dir inc in
        result.map (lam path. (info, normalizeFilePath path)) path
    ) includes in

    let results = map result.consume includedFiles in
    let warnings = flatMap fst results in
    let results = map snd results in
    let errors = flatten (eitherLefts results) in
    let files = eitherRights results in

    let program = optionMap (lam program. {
      program with
      decls = clearDeclIncludes program.decls
    }) program in

    let diagnostics = join [
      map (addSeverity (Error ())) errors,
      map (addSeverity (Warning ())) warnings
    ] in

    {
      program = program,
      links = files,
      diagnostics = diagnostics
    }
end