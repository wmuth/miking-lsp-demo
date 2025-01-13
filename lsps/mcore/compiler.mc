include "./util.mc"
include "./root.mc"
include "./utests.mc"
include "./main.mc"
include "./include-handler.mc"

lang MLangParser = MLangRoot
  sem parseMLang : Path -> String -> Result Diagnostic Diagnostic MLangProgram
  sem parseMLang path =| content ->
    let path = normalizeFilePath path in
    let dir = eraseFile path in

    let res = parseMLangString content in
    let res = result.map (populateMLangProgramInfoWithFilename path) res in
    let res = result.map (constTransformProgram builtin) res in
    let res = mapErrors (errorWithFilename path) res in
    res
end

lang MLangLinker = MLangRoot + MLangPathHandler
  type LinkerResult = {
    files: [MLangFile],
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }

  sem getDeclInclude : Decl -> Option (Info, Path)
  sem getDeclInclude =
  | _ -> None ()
  | DeclInclude { path = path, info = info } -> Some (info, path)

  sem getDeclIncludes : [Decl] -> [(Info, Path)]
  sem getDeclIncludes =| decls ->
    filterMap getDeclInclude decls  

  sem getIncludedFiles : (Path -> Option MLangFile) -> Path -> MLangFile -> LinkerResult
  sem getIncludedFiles getFile currentPath =| rootFile ->
    if leqi (length rootFile.includes) 0 then {
      files = [],
      errors = [],
      warnings = []
    } else

    let currentPath = normalizeFilePath currentPath in
    let dir = eraseFile currentPath in

    let includedFiles = map (
      lam inc.
        match inc with (info, path) in
        let path = findLocalPath dir (info, path) in
        let file = result.bind path (
          lam path.
            let file = getFile path in
            let file = optionMap (lam file. result.ok file) file in
            let file = optionGetOrElse (lam. result.err (info, join ["Include not found (", path, ")"])) file in
            let file = result.bind file (
              lam file.
                if geqi (length file.errors) 1 then
                  result.withAnnotations (result.warn (info, join ["Errors in included file (", path, ")"])) (result.ok file)
                else
                  result.ok file
            ) in
            file
        ) in
        file
    ) rootFile.includes in

    let files = filterMap result.toOption includedFiles in
    let problems = foldl1 result.withAnnotations includedFiles in

    let linkerResult: LinkerResult = {
      files = files,
      errors = match result.consume problems with (_, Left errors) then errors else [],
      warnings = match result.consume problems with (warnings, _) in warnings
    } in

    linkerResult
end

lang MLangCompiler = MLangRoot + MLangParser + MLangLinker + MLangLanguageServerCompiler

  -- Handle loading of dependent files
  sem createFileLoader: () -> (LSPCompilationParameters -> LSPCompilationResult)
  sem createFileLoader =| _ ->
    let cacheRef: Ref (Map URI MLangFile) = ref (mapEmpty cmpString) in

    recursive let fileLoader = lam getFile. lam uri. lam content.
      let cache = deref cacheRef in

      let oldFile = mapLookup uri cache in
      let file = compileMLangLSP getFile oldFile uri content in

      let languageSupport = fileToLanguageSupport file in

      (file, languageSupport)
    in

    lam parameters.
      let uri = stripUriProtocol parameters.uri in
      let content = parameters.content in

      modref cacheRef (mapRemove uri (deref cacheRef));
      let languageSupportBuffer: Ref (Map URI [LanguageServerPayload]) = ref (mapEmpty cmpString) in

      recursive let getFile : Option String -> Path -> Option MLangFile = lam content. lam path.
        let cache = deref cacheRef in
        match mapLookup path cache with Some file then
          Some file
        else
          let readFromFile = lam. match optionMap fileReadString (fileReadOpen path) with Some content in content in
          let content = optionGetOrElse readFromFile content in
          match fileLoader (getFile (None ())) path content with (file, languageSupport) in

          modref languageSupportBuffer (mapInsert path languageSupport (deref languageSupportBuffer));
          modref cacheRef (mapInsert path file cache);
          Some file
      in

      getFile (Some content) uri;
      deref languageSupportBuffer

  sem compileMLangLSP: (Path -> Option MLangFile) -> Option MLangFile -> Path -> String -> MLangFile
  sem compileMLangLSP getFile oldFile uri =| content ->
    let file = {
      emptyMLangFile with
      content = content,
      filename = uri
    } in

    let parsedProgramResult = parseMLang uri content in

    match result.consume parsedProgramResult with (warnings, parsedProgramEither) in

    let file: MLangFile = switch parsedProgramEither
      case Left errors then
        {
          file with
          errors = errors,
          warnings = warnings
        }
      case Right parsedProgram then
        {
          file with
          program = Some parsedProgram,
          includes = getDeclIncludes parsedProgram.decls,
          errors = [],
          warnings = warnings
        }
    end in

    let includes = getIncludedFiles getFile uri file in

    let file = {
      file with
      errors = join [
        file.errors,
        includes.errors
      ],
      warnings = join [
        file.warnings,
        includes.warnings
      ]
    } in

    file
end