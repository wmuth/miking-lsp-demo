include "mlang/main.mc"

include "./file.mc"
include "./util.mc"

lang MLangIncludeChecker = MLangPipeline
  sem getDeclIncludes : Decl -> Option Info
  sem getDeclIncludes =
  | _ -> None ()
  | DeclInclude { info = info } -> Some info    
end

lang MLangParser = MLangFileHandler
  sem parseMLang : MLangFile -> Path -> String -> MLangFile
  sem parseMLang file path =| content ->
    let path = normalizeFilePath path in
    let dir = eraseFile path in

    let res = result.map (populateMLangProgramInfoWithFilename path) (use BootParserMLang in parseMLangString content) in
    let res = result.map (use MLangPipeline in constTransformProgram builtin) res in
    let res = mapErrors (errorWithFilename path) res in

    match result.consume res with (warnings, parsedResult) in
    switch parsedResult
      case Left errors then CParseError {
        loaded = { content = content, filename = getFilename file },
        errors = errors,
        warnings = warnings
      }
      case Right program then
        let includes = use MLangIncludeChecker in filterMap getDeclIncludes program.decls in
        if geqi (length includes) 1 then
          CParseError {
            loaded = { content = content, filename = getFilename file },
            errors = map (lam info. (info, "Includes are not allowed in this MCore language server version (sorry)")) includes,
            warnings = warnings
          }
        else
          CParsed {
            loaded = { content = content, filename = getFilename file },
            program = program,
            errors = [],
            warnings = warnings
          }
    end
end