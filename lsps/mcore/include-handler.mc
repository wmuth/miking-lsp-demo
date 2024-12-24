include "mlang/main.mc"
include "../../lsp-server/lsp/root.mc"
include "./util.mc"

let libs = addCWDtoLibs (parseMCoreLibsEnv ())

type LocalPath = String
type Path = String

lang MLangAndMExpr = MLangAst + MExprAst
end

lang MLangCompilationKind = MLangAndMExpr
  -- We store not found included files,
  -- so that when a non-existing file is created,
  -- we can update the diagnostics of all dependents (dirty paths handling).
  syn Include =
  | ExistingFile Path
  | NotFound [Path]

  sem inc2str: Include -> Option String
  sem inc2str =
  | ExistingFile path -> Some (normalizeFilePath path)
  | NotFound paths -> None ()

  syn MLangFileKind =
  | Loaded { content: String }
  | Parsed { content: String, parsed: MLangProgram, includes: [(Info, Include)] }
end

-- Set the filename of the info for an error
let errorWithFilename: String -> Diagnostic -> Diagnostic =
  lam filename. lam err.
    match err with (info, msg) in
      (infoWithFilename filename info, msg)

let createMultipleFilesFoundWarning = lam existingFilesAsSet.
  let sep = "\n * " in
  join [
    "Multiple files found: ",
    sep,
    strJoin sep (setToSeq existingFilesAsSet),
    "\nUsing: '",
    head (setToSeq existingFilesAsSet),
    "'"
  ]

type MLangFile = {
  kind: use MLangCompilationKind in MLangFileKind,
  errors: [Diagnostic],
  warnings: [Diagnostic]
}

lang MLangPaths = MLangCompilationKind
  syn PathStatus =
  | Found String
  | NotFound (String, [Path]) -- (path, searchPaths)
  | MultipleFound [String]

  sem getIncludeFromPathStatus : PathStatus -> Include
  sem getIncludeFromPathStatus =
  | Found path -> ExistingFile path
  | NotFound (_, paths) -> use MLangCompilationKind in NotFound paths
  | MultipleFound paths -> ExistingFile (head paths)

  sem findPaths : String -> Map String String -> [(Info, LocalPath)] -> ([Diagnostic], [Diagnostic], [(Info, Include)])
  sem findPaths dir libs =| includes ->
    let includes = map (findPath dir libs) includes in
    let includePaths = map (lam v. (v.0, getIncludeFromPathStatus v.1)) includes in

    let errors = filterMap (
      lam infoPathStatus.
        match infoPathStatus with (info, pathStatus) in
        switch pathStatus
          case NotFound (path, searchPaths) then Some (info, join ["File not found: '", path, "'\n Tried: \n* ", strJoin "\n* " searchPaths])
          case _ then None ()
        end
    ) includes in

    let warnings = filterMap (
      lam infoPathStatus.
        match infoPathStatus with (info, pathStatus) in
        switch pathStatus
          case MultipleFound paths then Some (info, createMultipleFilesFoundWarning (setOfSeq cmpString paths))
          case _ then None ()
        end
    ) includes in

    (errors, warnings, includePaths)

  sem findPath : String -> Map String String -> (Info, LocalPath) -> (Info, PathStatus)
  sem findPath dir libs =| infoPath ->
    match infoPath with (info, path) in
    let libs = mapInsert "current" dir libs in
    let prefixes = mapValues libs in 
    let paths = map (lam prefix. filepathConcat prefix path) prefixes in

    let existingFiles = filter sysFileExists paths in 
    let existingFilesAsSet = setOfSeq cmpString existingFiles in 

    let res = switch (setSize existingFilesAsSet)
      case 0 then 
        NotFound (path, paths)
      case 1 then 
        Found (head (setToSeq existingFilesAsSet))
      case _ then 
        -- TODO(voorberg, 09/05/2024): This happens because we dont properly
        -- deal with libraries yet. The code does not yet realise that 
        -- some absolute path is equal to some relative path.
        -- let warning = result.warn (info, createMultipleFilesFoundWarning existingFilesAsSet) in
        MultipleFound (setToSeq existingFilesAsSet)
    end in

    (info, res)
end

lang MLangCompilation = MLangCompilationKind + MLangPaths + MLangAst + BootParserMLang
  sem parseMLang : Path -> String -> MLangFile
  sem parseMLang path =| content ->
    let path = normalizeFilePath path in
    let dir = eraseFile path in

    let res = result.map (populateMLangProgramInfoWithFilename path) (use BootParserMLang in parseMLangString content) in
    let res = mapErrors (errorWithFilename path) res in
    match result.consume res with (warnings, parsedResult) in
    switch parsedResult
      case Left errs then {
        kind = Loaded { content = content },
        errors = errs,
        warnings = warnings
      }
      case Right parsed then
        let includes = extractIncludes parsed.decls in
        match findPaths dir libs includes with (includeErrors, includeWarnings, includes) in

        {
          kind = Parsed { content = content, parsed = parsed, includes = includes },
          errors = includeErrors,
          warnings = join [warnings, includeWarnings]
        }
    end

  sem extractIncludes : [Decl] -> [(Info, LocalPath)]
  sem extractIncludes =
  | [] -> []
  | [DeclInclude {info = info, path = path}] ++ decls ->
    let path = normalizeFilePath path in
    let includes = extractIncludes decls in
    join [[(info, path)], includes]
  | [_] ++ decls ->
    extractIncludes decls
end

type MLangProgramResult = use MLangAst in Result Diagnostic Diagnostic MLangProgram

-- type MLangFile = {
--   content: String,
--   parsed: Option MLangProgramResult,
--   symEnv: Option SymEnv
-- }

-- let emptyFile = {
--   content = "",
--   parsed = None (),
--   includeCache = mapEmpty cmpString,
--   symEnv = None ()
-- }

type FileLoader = {
  load: Info -> String -> Result Diagnostic Diagnostic MLangFile,
  store: String -> MLangFile -> ()
}

-- lang MLangModularIncludeHandler = MLangAst + BootParserMLang
--   sem parseAndHandleIncludes : FileLoader -> String -> MLangProgramResult
--   sem parseAndHandleIncludes loader =| path -> 
--     let dir = eraseFile path in 
--     let libs = addCWDtoLibs (parseMCoreLibsEnv ()) in
--     let included = ref (setEmpty cmpString) in

--     match result.consume (loader.load testinfo_ path) with (_, Right file) in
--     handleIncludesFile loader included dir libs path file

--   sem handleIncludesProgram : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgram -> MLangProgramResult 
--   sem handleIncludesProgram loader included dir libs path file =| prog ->
--     let f = lam decl.
--       let result = flattenIncludes loader included dir libs file path decl in
--       -- loader.store path {
--       --   file with
--       --   includeCache = mapInsert (normalizeFilePath path) result file.includeCache
--       -- };
--       result
--     in
--     let decls = map f prog.decls in
--     let decls = flattenErrors decls in
--     let consumeDecls = lam decls.
--       let f = lam decls. lam decl. concat decls decl in
--       {prog with decls = foldl f [] decls}
--     in
--     result.map consumeDecls decls

--   sem handleIncludesFile : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgramResult
--   sem handleIncludesFile loader included dir libs path =| file ->
--     let path = normalizeFilePath path in
--     let s = deref included in 

--     if setMem path s then 
--       result.ok {decls = [], expr = uunit_}
--     else 
--       let handleProgram = lam prog.
--         modref included (setInsert path s);
--         handleIncludesProgram loader included dir libs path file prog
--       in

--       let parseMLangString = lam file. lam path.
--         let orElse = lam.
--           let res = result.map (populateMLangProgramInfoWithFilename path) (parseMLangString file.content) in
--           let res = mapErrors (errorWithFilename path) res in
--           loader.store path {
--             file with
--             parsed = Some res
--           };
--           res
--         in

--         (
--           match file.parsed
--             with Some parsed then eprintln (join ["Already parsed: ", path])
--             else eprintln (join ["Parsing: ", path])
--         );
        
--         optionGetOrElse orElse file.parsed
--       in

--       result.bind (parseMLangString file path) handleProgram

--   sem flattenIncludes : FileLoader -> Ref (Set String) -> String -> Map String String -> MLangFile -> String -> Decl -> Result Diagnostic Diagnostic [String]
--   sem flattenIncludes loader included dir libs file childPath =
--   | DeclInclude {path = path, info = info} ->
--     let consumePath = lam path.
--       let path = normalizeFilePath path in
--       loader.addDependency path childPath;

--       eprintln (join ["[", childPath ,"] Including: ", path]);
--       match mapLookup path file.includeCache with Some included then
--         eprintln (join ["[", childPath ,"] Cache hit: ", path]);
--         included
--       else
--         let consumeContent = lam file.
--           let program = handleIncludesFile loader included (eraseFile path) libs path file in
--           result.map (lam prog. prog.decls) program
--         in
--         let res = result.bind (loader.load info path) consumeContent in
--         eprintln (join ["[", childPath ,"] Adding cache: ", path]);
--         loader.store (normalizeFilePath childPath) {
--           file with
--           includeCache = mapInsert path res file.includeCache
--         };
--         res
--     in

--     let completePath = findPath loader dir libs info path in
--     let decls = result.bind completePath consumePath in

--     match result.consume decls with (_, declsResult) in
--     switch declsResult
--       case Right declsOk then
--         result.withAnnotations decls (result.ok declsOk)
--       case Left _ then
--         let additionalError = result.err (info, join [
--           "File '",
--           path,
--           "' could not be parsed!"
--         ]) in
--         result.map2 (lam a1. lam a2. join [a1, a2]) decls additionalError
--     end
--   | other -> result.ok [other]

--   sem findPath : FileLoader -> String -> Map String String -> Info -> String -> Result Diagnostic Diagnostic String
--   sem findPath loader dir libs info =| path ->
--     let libs = mapInsert "current" dir libs in
--     let prefixes = mapValues libs in 
--     let paths = map (lam prefix. filepathConcat prefix path) prefixes in 

--     let existingFiles = filter sysFileExists paths in 
--     let existingFilesAsSet = setOfSeq cmpString existingFiles in 

--     switch (setSize existingFilesAsSet)
--       case 0 then 
--         result.err (info, "File not found!")
--       case 1 then 
--         result.ok (head (setToSeq existingFilesAsSet))
--       case _ then 
--         -- TODO(voorberg, 09/05/2024): This happens because we dont properly
--         -- deal with libraries yet. The code does not yet realise that 
--         -- some absolute path is equal to some relative path.
--         let warning = result.warn (info, createMultipleFilesFoundWarning existingFilesAsSet) in
--         result.withAnnotations warning (result.ok (head (setToSeq existingFilesAsSet)))
--     end
-- end