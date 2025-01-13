include "./util.mc"
include "./root.mc"
include "./utests.mc"
include "./main.mc"

lang MLangParser = MLangRoot
  sem getDeclIncludes : Decl -> Option Info
  sem getDeclIncludes =
  | _ -> None ()
  | DeclInclude { info = info } -> Some info

  sem parseMLang : Path -> String -> Result Diagnostic Diagnostic MLangProgram
  sem parseMLang path =| content ->
    let path = normalizeFilePath path in
    let dir = eraseFile path in

    let res = parseMLangString content in
    let res = result.map (populateMLangProgramInfoWithFilename path) res in
    let res = result.map (constTransformProgram builtin) res in
    let res = mapErrors (errorWithFilename path) res in
    result.bind res (
      lam program.
        let includes = filterMap getDeclIncludes program.decls in
        if geqi (length includes) 1 then
          let info = head includes in
          result.err (info, "Includes are not allowed in this MCore language server version (sorry.)")
        else
          result.ok program
    )
end

lang MLangCompiler = MLangRoot + MLangParser
  sem compileMLangLSP: CompilationParameters -> [LanguageServerPayload]
  sem compileMLangLSP =| parameters ->
    let uri = stripUriProtocol parameters.uri in
    let content = parameters.content in
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
          errors = [],
          warnings = warnings
        }
    end in

    join [
      use MLangLookupVariable in fileToLanguageSupport file,
      map (lam diagnostic. LsDiagnostic { location=diagnostic.0, message=diagnostic.1, severity=Error () }) (file.errors),
      map (lam diagnostic. LsDiagnostic { location=diagnostic.0, message=diagnostic.1, severity=Warning () }) (file.warnings)
    ]
end