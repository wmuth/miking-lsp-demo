include "mlang/main.mc"

include "./root.mc"

let libs = addCWDtoLibs (parseMCoreLibsEnv ())


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

lang MLangPathHandler = MLangRoot
  type LocalPath = Path

  syn PathStatus =
  | Found String
  | NotFound (String, [Path]) -- (path, searchPaths)
  | MultipleFound [String]

  sem findLocalPath : String -> (Info, LocalPath) -> Result Diagnostic Diagnostic Path
  sem findLocalPath dir =| pathInfo ->
    match pathInfo with (info, path) in
    switch findMLangPath dir libs path
      case Found foundPath then
        result.ok foundPath
      case NotFound (path, searchPaths) then
        result.err (info, join ["File not found: '", path, "'\n Tried: \n* ", strJoin "\n* " (map normalizeFilePath searchPaths)])
      case MultipleFound ([path] ++ paths) then
        result.withAnnotations
          (result.warn (info, createMultipleFilesFoundWarning (setOfSeq cmpString (join [[path], paths]))))
          (result.ok path)
    end

  sem findMLangPath : String -> Map String String -> LocalPath -> PathStatus
  sem findMLangPath dir libs =| path ->
    let libs = mapInsert "current" dir libs in
    let prefixes = mapValues libs in
    let paths = map (lam prefix. filepathConcat prefix path) prefixes in

    let existingFiles = filter sysFileExists paths in
    let existingFilesAsSet = setOfSeq cmpString existingFiles in

    switch setToSeq existingFilesAsSet
      case [] then
        NotFound (path, paths)
      case [foundPath] then
        Found foundPath
      case multiplePaths then
        -- TODO(voorberg, 09/05/2024): This happens because we dont properly
        -- deal with libraries yet. The code does not yet realise that 
        -- some absolute path is equal to some relative path.
        -- let warning = result.warn (info, createMultipleFilesFoundWarning existingFilesAsSet) in
        MultipleFound multiplePaths
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