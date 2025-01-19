include "./util.mc"
include "./root.mc"
include "./utests.mc"
include "./main.mc"
include "./include-handler.mc"

lang MLangParser = MLangRoot
  sem getDeclInclude : Decl -> Option Link
  sem getDeclInclude =
  | _ -> None ()
  | DeclInclude { path = path, info = info } -> Some (info, path)

  sem getDeclIncludes : [Decl] -> [Link]
  sem getDeclIncludes =| decls ->
    filterMap getDeclInclude decls  

  sem getPathFromLink : Link -> Path
  sem getPathFromLink =| (info, path) -> path

  sem _parseMLang : Path -> String -> Result Diagnostic Diagnostic MLangProgram
  sem _parseMLang path =| content ->
    let path = normalizeFilePath path in
    let dir = eraseFile path in

    let res = parseMLangString content in
    let res = result.map (populateMLangProgramInfoWithFilename path) res in
    let res = result.map (constTransformProgram builtin) res in
    let res = mapErrors (errorWithFilename path) res in

    res

  sem lsParseMLang : Path -> String -> MLangParsedFile
  sem lsParseMLang filename =| content ->
    let parsedProgramResult = _parseMLang filename content in

    match result.consume parsedProgramResult with (warnings, parsedProgramEither) in

    let parsedProgram = switch parsedProgramEither
      case Left errors then
        {
          program = None (),
          includes = [],
          diagnostics = join [
            map (addSeverity (Error ())) errors,
            map (addSeverity (Warning ())) warnings
          ]
        }
      case Right parsedProgram then
        let includes = getDeclIncludes parsedProgram.decls in

        {
          program = Some parsedProgram,
          includes = includes,
          diagnostics = join [
            map (addSeverity (Warning ())) warnings
          ]
        }
    end in

    parsedProgram
end

lang MLangLinker = MLangRoot + MLangPathHandler
  sem lsLinkMLang : Path -> [Link] -> MLangLinkedFile
  sem lsLinkMLang filename =| includes ->
    let currentPath = normalizeFilePath filename in
    let dir = eraseFile currentPath in

    let includedFiles: [Result Diagnostic Diagnostic Link] = map (
      lam inc.
        match inc with (info, path) in
        let path = findLocalPath dir inc in
        result.map (lam path. (info, path)) path
    ) includes in

    let results = map result.consume includedFiles in
    let warnings = flatMap fst results in
    let results = map snd results in
    let errors = flatten (eitherLefts results) in
    let files = eitherRights results in

    let diagnostics = join [
      map (addSeverity (Error ())) errors,
      map (addSeverity (Warning ())) warnings
    ] in

    {
      includes = files,
      diagnostics = diagnostics
    }
end

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

  sem lsSymbolizeMLang : (Path -> Option MLangFile) -> Path -> [Link] -> Option MLangProgram -> MLangSymbolizedFile
  sem lsSymbolizeMLang getFile filename includes =| program ->
    let linkerResult = getIncludedFiles getFile includes in

    let symEnv = symEnvEmpty in
    let symbolizedProgram = program in -- todo

    {
      program = symbolizedProgram,
      symEnv = symEnv,
      diagnostics = join [
        linkerResult.diagnostics
      ]
    }
end

lang MLangCompiler =
  MLangRoot + MLangLanguageServerCompiler +
  MLangParser + MLangSymbolizer

  sem createReversedDependencies : Map URI (Set URI) -> Map URI (Set URI)
  sem createReversedDependencies =| dependencies ->
    -- Convert the map to a sequence
    let seq: [(Path, Set Path)] = mapToSeq dependencies in
    -- Create a list of pairs (childUri, parentUri)
    let pairs: [(Path, Path)] = flatMap (
      lam dependency.
        match dependency with (childUri, parentUris) in
        map (lam parentUri. (childUri, parentUri)) (setToSeq parentUris)
    ) seq in
    
    -- Group the pairs by the parentUri
    let flatDependencyGraph: Map URI (Set URI) = foldl (
      lam acc. lam dependency.
        match dependency with (childUri, parentUri) in
        mapInsertWith setUnion parentUri (setSingleton cmpString childUri) acc
    ) (mapEmpty cmpString) pairs in

    -- TODO: Implement parentUri having all children
    let reversedDependencies: Map URI (Set URI) = flatDependencyGraph in
    reversedDependencies

  -- Handle loading of dependent files
  sem createFileLoader: () -> (LSPCompilationParameters -> LSPCompilationResult)
  sem createFileLoader =| _ ->
    let cacheRef: Ref (Map URI MLangFile) = ref (mapEmpty cmpString) in
    let dependencies: Ref (Map URI (Set URI)) = ref (mapEmpty cmpString) in
    let reversedDependencies: Ref (Map URI (Set URI)) = ref (mapEmpty cmpString) in

    lam parameters.
      let languageSupportBuffer: Ref (Map URI [LanguageServerPayload]) = ref (mapEmpty cmpString) in
      let dirtyFilesBuffer: Ref (Set URI) = ref (setEmpty cmpString) in

      recursive let fileLoader: (Path -> Option MLangFile) -> MLangFile -> MLangFile =
        lam getFile. lam file.
          let cache = deref cacheRef in

          let path = file.filename in
          let content = file.content in
  
          -- let file = optionGetOrElse (lam. createEmptyFile path content) (mapLookup path cache) in
          let file = compileMLangLSP getFile file path content in
  
          let languageSupport = fileToLanguageSupport file in
  
          let dirtyFiles = mapLookupOr (setEmpty cmpString) path (deref reversedDependencies) in
          eprintln (join ["Dirty files: ", strJoin ", " (setToSeq dirtyFiles)]);
          let newDependencies = optionMapOr [] (lam linked. linked.includes) file.linked in
          let newDependencies: Set Path = setOfSeq cmpString (map getPathFromLink newDependencies) in

          modref dependencies (mapInsert path newDependencies (deref dependencies));
          modref reversedDependencies (createReversedDependencies (deref dependencies));
          modref languageSupportBuffer (mapInsert path languageSupport (deref languageSupportBuffer));
          modref cacheRef (mapInsert path file cache);
          modref dirtyFilesBuffer (setUnion dirtyFiles (setSingleton cmpString path));
  
          file
      in

      let path = stripUriProtocol parameters.uri in
      let content = parameters.content in

      -- Mark the current changed file as changed, making
      -- it ellible for re-parsing.
      modref cacheRef (mapInsertWith (lam file. lam val. {
        file with
        status = Changed (),
        content = val.content
      }) path (createEmptyFile path content) (deref cacheRef));

      recursive let getFile : Path -> Option MLangFile =
        lam path.
          let cache = deref cacheRef in

          let file = match mapLookup path cache
            with Some file then
              file
            else
              match optionMap fileReadString (fileReadOpen path) with Some content in
              createEmptyFile path content
          in

          -- Don't load the file if it's already been symbolized
          let file = match file.status
            with Symbolized () then
              file
            else
              fileLoader getFile file
          in

          Some file
      in

      -- Get the main file
      getFile path;
      
      -- Mark all dirty files as dirty, making them ellible for re-linking
      -- and re-symbolization.
      iter (
        lam dirtyFilePath.
          let maybeFile = mapLookup dirtyFilePath (deref cacheRef) in
          optionMap (
            lam file.
              let file = {
                file with
                status = Dirty ()
              } in
              modref cacheRef (mapInsert dirtyFilePath file (deref cacheRef))
          ) maybeFile; ()
      ) (setToSeq (deref dirtyFilesBuffer));

      let seenDirtyFiles: Ref (Set URI) = ref (setEmpty cmpString) in
      iter (lam path. getFile path; ()) (setToSeq (deref dirtyFilesBuffer));

      deref languageSupportBuffer

  sem compileMLangLSP: (Path -> Option MLangFile) -> MLangFile -> Path -> String -> MLangFile
  sem compileMLangLSP getFile file uri =| content ->
    let parsed = match (file.parsed, file.status)
      with (Some parsed, !Changed ()) then parsed
      else lsParseMLang file.filename file.content
    in

    let linked = match (file.linked, file.status)
      with (Some linked, Linked () | Symbolized ()) then linked
      else lsLinkMLang file.filename parsed.includes
    in

    let symbolized = match (file.symbolized, file.status)
      with (Some symbolized, Symbolized ()) then symbolized
      else lsSymbolizeMLang getFile file.filename linked.includes parsed.program
    in

    let file: MLangFile = {
      file with
      status = Symbolized (),
      parsed = Some parsed,
      linked = Some linked,
      symbolized = Some symbolized
    } in

    file
end