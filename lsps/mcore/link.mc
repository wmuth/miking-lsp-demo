include "file.mc"

lang MLangLink = MLangFileHandler
  sem removeIncludeDecls : use MLangIncludeHandler in [Decl] -> [Decl]
  sem removeIncludeDecls =
  | [] -> []
  | [DeclInclude _] ++ rest -> removeIncludeDecls rest
  | [decl] ++ rest -> join [[decl], removeIncludeDecls rest]

  sem linkMLang : (Path -> MLangFile) -> MLangFile -> MLangFile
  sem linkMLang getFile =
  | file & CParsed parsed ->
    let f = lam pathInfo.
      match pathInfo with (info, path, file) in
      if leqi (length (getFileErrors file)) 0 then
        None ()
      else
        Some (info, join ["File '", path, "' contains errors!"])
    in
    
    let links = map (lam v. (v.0, v.1, getFile v.1)) (getIncludePaths file) in
    let linkErrors = filterOption (map f links) in

    let program = {
      parsed.program with
      decls = removeIncludeDecls parsed.program.decls
    } in

    CLinked {
      program = program,
      parsed = parsed,
      links = links,
      linkErrors = linkErrors,
      warnings = []
    }
end