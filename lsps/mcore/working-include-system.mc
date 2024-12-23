lang MLangModularIncludeHandler = MLangAst + BootParserMLang
  sem parseAndHandleIncludes : FileLoader -> String -> MLangProgramResult
  sem parseAndHandleIncludes loader =| path -> 
    let dir = eraseFile path in 
    let libs = addCWDtoLibs (parseMCoreLibsEnv ()) in
    let included = ref (setEmpty cmpString) in

    match result.consume (loader.load testinfo_ path) with (_, Right file) in
    handleIncludesFile loader included dir libs path file

  sem handleIncludesProgram : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgram -> MLangProgramResult 
  sem handleIncludesProgram loader included dir libs path file =| prog ->
    let f = lam decl.
      let result = flattenIncludes loader included dir libs file path decl in
      -- loader.store path {
      --   file with
      --   includeCache = mapInsert (normalizeFilePath path) result file.includeCache
      -- };
      result
    in
    let decls = map f prog.decls in
    let decls = flattenErrors decls in
    let consumeDecls = lam decls.
      let f = lam decls. lam decl. concat decls decl in
      {prog with decls = foldl f [] decls}
    in
    result.map consumeDecls decls

  sem handleIncludesFile : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgramResult
  sem handleIncludesFile loader included dir libs path =| file ->
    let path = normalizeFilePath path in
    let s = deref included in 

    if setMem path s then 
      result.ok {decls = [], expr = uunit_}
    else 
      let handleProgram = lam prog.
        modref included (setInsert path s);
        handleIncludesProgram loader included dir libs path file prog
      in

      let parseMLangString = lam file. lam path.
        let orElse = lam.
          let res = result.map (populateMLangProgramInfoWithFilename path) (parseMLangString file.content) in
          let res = mapErrors (errorWithFilename path) res in
          loader.store path {
            file with
            parsed = Some res
          };
          res
        in

        (
          match file.parsed
            with Some parsed then eprintln (join ["Already parsed: ", path])
            else eprintln (join ["Parsing: ", path])
        );
        
        optionGetOrElse orElse file.parsed
      in

      result.bind (parseMLangString file path) handleProgram

  sem flattenIncludes : FileLoader -> Ref (Set String) -> String -> Map String String -> MLangFile -> String -> Decl -> Result Diagnostic Diagnostic [Decl]
  sem flattenIncludes loader included dir libs file childPath =
  | DeclInclude {path = path, info = info} ->
    let consumePath = lam path.
      let path = normalizeFilePath path in
      loader.addDependency path childPath;

      eprintln (join ["[", childPath ,"] Including: ", path]);
      match mapLookup path file.includeCache with Some included then
        eprintln (join ["[", childPath ,"] Cache hit: ", path]);
        included
      else
        let consumeContent = lam file.
          let program = handleIncludesFile loader included (eraseFile path) libs path file in
          result.map (lam prog. prog.decls) program
        in
        let res = result.bind (loader.load info path) consumeContent in
        eprintln (join ["[", childPath ,"] Adding cache: ", path]);
        loader.store (normalizeFilePath childPath) {
          file with
          includeCache = mapInsert path res file.includeCache
        };
        res
    in

    let completePath = findPath loader dir libs info path in
    let decls = result.bind completePath consumePath in

    match result.consume decls with (_, declsResult) in
    switch declsResult
      case Right declsOk then
        result.withAnnotations decls (result.ok declsOk)
      case Left _ then
        let additionalError = result.err (info, join [
          "File '",
          path,
          "' could not be parsed!"
        ]) in
        result.map2 (lam a1. lam a2. join [a1, a2]) decls additionalError
    end
  | other -> result.ok [other]

  sem findPath : FileLoader -> String -> Map String String -> Info -> String -> Result Diagnostic Diagnostic String
  sem findPath loader dir libs info =| path ->
    let libs = mapInsert "current" dir libs in
    let prefixes = mapValues libs in 
    let paths = map (lam prefix. filepathConcat prefix path) prefixes in 

    let existingFiles = filter sysFileExists paths in 
    let existingFilesAsSet = setOfSeq cmpString existingFiles in 

    switch (setSize existingFilesAsSet)
      case 0 then 
        result.err (info, "File not found!")
      case 1 then 
        result.ok (head (setToSeq existingFilesAsSet))
      case _ then 
        -- TODO(voorberg, 09/05/2024): This happens because we dont properly
        -- deal with libraries yet. The code does not yet realise that 
        -- some absolute path is equal to some relative path.
        let warning = result.warn (info, createMultipleFilesFoundWarning existingFilesAsSet) in
        result.withAnnotations warning (result.ok (head (setToSeq existingFilesAsSet)))
    end
end