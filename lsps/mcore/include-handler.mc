include "mlang/main.mc"
include "../../lsp-server/lsp/root.mc"
include "./util.mc"

type LocalPath = String
type Path = String

let libs = addCWDtoLibs (parseMCoreLibsEnv ())

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

lang MLangPathHandler = MLangAst
  syn PathStatus =
  | Found String
  | NotFound (String, [Path]) -- (path, searchPaths)
  | MultipleFound [String]

  sem findPaths : String -> Map String String -> [(Info, LocalPath)] -> ([Diagnostic], [Diagnostic], [(Info, PathStatus)])
  sem findPaths dir libs =| includes ->
    let includes = map (findMLangPath dir libs) includes in

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

    (errors, warnings, includes)

  sem findMLangPath : String -> Map String String -> (Info, LocalPath) -> (Info, PathStatus)
  sem findMLangPath dir libs =| infoPath ->
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

lang MLangIncludeHandler = MLangPathHandler
  -- We store not found included files,
  -- so that when a non-existing file is created,
  -- we can update the diagnostics of all dependents (dirty paths handling).
  syn Include =
  | ExistingFile Path
  | NonExistentFiles [Path]

  sem inc2str: Include -> Option String
  sem inc2str =
  | ExistingFile path -> Some (normalizeFilePath path)
  | NonExistentFiles paths -> None ()

  sem getIncludeFromPathStatus : PathStatus -> Include
  sem getIncludeFromPathStatus =
  | Found path -> ExistingFile path
  | NotFound (_, paths) -> NonExistentFiles paths
  | MultipleFound paths -> ExistingFile (head paths)
end