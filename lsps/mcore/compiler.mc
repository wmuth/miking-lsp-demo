include "./root.mc"
include "./main.mc"
include "./parser.mc"
include "./linker.mc"
include "./symbolize.mc"
include "./mexpr.mc"

lang MLangCompiler =
  MLangRoot + MLangLanguageServerCompiler +
  MLangParser + MLangSymbolizer + MLangMExprCompiler

  syn EventType =
  | Open
  | Change

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

  sem createEmptyFileFromDisk : Path -> MLangFile
  sem createEmptyFileFromDisk =| path ->
    match optionMap fileReadString (fileReadOpen path) with Some content in
    createEmptyFile path content

  -- Handle loading of dependent files
  sem createFileLoader: EventType -> (LSPCompilationParameters -> LSPCompilationResult)
  sem createFileLoader =| eventType ->
    let cacheRef: Ref (Map URI MLangFile) = ref (mapEmpty cmpString) in
    let dependencies: Ref (Map URI (Set URI)) = ref (mapEmpty cmpString) in
    let reversedDependencies: Ref (Map URI (Set URI)) = ref (mapEmpty cmpString) in

    lam parameters: LSPCompilationParameters.
      let languageSupportBuffer: Ref (Map URI [LanguageServerPayload]) = ref (mapEmpty cmpString) in
      let dirtyFilesBuffer: Ref (Set URI) = ref (setEmpty cmpString) in

      recursive let fileLoader: (Path -> Option MLangFile) -> MLangFile -> MLangFile =
        lam getFile. lam file.
          let path = file.filename in
          let content = file.content in
          let file = compileMLangLSP getFile file path content in
          let languageSupport = fileToLanguageSupport file in
  
          let dirtyFiles = mapLookupOr (setEmpty cmpString) path (deref reversedDependencies) in
          -- eprintln (join ["Dirty files: ", strJoin ", " (setToSeq dirtyFiles)]);
          let newDependencies = optionMapOr [] (lam linked. linked.links) file.linked in
          let newDependencies: Set Path = setOfSeq cmpString (map getPathFromLink newDependencies) in

          modref dependencies (mapInsert path newDependencies (deref dependencies));
          modref reversedDependencies (createReversedDependencies (deref dependencies));
          modref languageSupportBuffer (mapInsert path languageSupport (deref languageSupportBuffer));
          modref cacheRef (mapInsert path file (deref cacheRef));
          modref dirtyFilesBuffer (setUnion dirtyFiles (setSingleton cmpString path));
  
          file
      in

      let path = stripUriProtocol parameters.uri in
      let content = parameters.content in

      -- Mark the current changed file as changed, making
      -- it ellible for re-parsing.
      (match eventType with Change () then
        modref cacheRef (mapInsertWith (lam file. lam val. {
          file with
          status = Changed (),
          content = val.content
        }) path (createEmptyFile path content) (deref cacheRef))
      else ());

      recursive let getFile : Path -> Option MLangFile =
        lam path.
          let cache = deref cacheRef in
          let file = optionGetOrElse (lam. createEmptyFileFromDisk path) (mapLookup path cache) in

          -- Don't load the file if it's already been symbolized
          let file = match file.status
            with Symbolized () | TypeChecked () then file
            else fileLoader getFile file
          in

          Some file
      in

      -- Get the main file (main entry point)
      getFile path;
      
      recursive let markFileAsDirty = lam path: String.
        match mapLookup path (deref cacheRef) with
          Some file then
            let file = {
              file with
              status = Dirty ()
            } in
            let childDirtyFiles = mapLookupOr (setEmpty cmpString) path (deref reversedDependencies) in
            iter markFileAsDirty (setToSeq childDirtyFiles);
            modref dirtyFilesBuffer (setUnion (deref dirtyFilesBuffer) (setSingleton cmpString path));
            modref cacheRef (mapInsert path file (deref cacheRef))
          else ()
      in

      -- Mark all dirty files as dirty, making them ellible for re-linking and re-symbolization.
      iter markFileAsDirty (setToSeq (deref dirtyFilesBuffer));
      -- Trigger re-linking and re-symbolization of all dirty files
      iter (lam path. getFile path; ()) (setToSeq (deref dirtyFilesBuffer));

      -- Return all cached language support results
      deref languageSupportBuffer

  sem compileMLangLSP: (Path -> Option MLangFile) -> MLangFile -> Path -> String -> MLangFile
  sem compileMLangLSP getFile file uri =| content ->
    let parsed = match (file.parsed, file.status)
      with (Some parsed, !Changed ()) then parsed
      else lsParseMLang file.filename file.content
    in

    let linked = match (file.linked, file.status)
      with (Some linked, !Changed () & !Parsed ()) then linked
      else lsLinkMLang file.filename parsed.includes parsed.program
    in

    let symbolized = match (file.symbolized, file.status)
      with (Some symbolized, Symbolized () | TypeChecked ()) then symbolized
      else lsSymbolizeMLang getFile file.filename linked.links linked.program
    in

    let typeChecked = match (file.typeChecked, file.status)
      with (Some typeChecked, TypeChecked ()) then typeChecked
      else lsCompileMLangToMExpr getFile file.filename linked.links symbolized.program
    in

    let file: MLangFile = {
      file with
      status = TypeChecked (),
      -- status = Symbolized (),
      parsed = Some parsed,
      linked = Some linked,
      symbolized = Some symbolized,
      typeChecked = Some typeChecked
    } in

    file
end