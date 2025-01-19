include "./util.mc"
include "./root.mc"

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