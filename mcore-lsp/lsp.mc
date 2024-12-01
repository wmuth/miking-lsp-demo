include "../miking-lsp/dsl/lsp-server.mc"
include "../../miking/src/main/eval.mc"

include "./compile-mcore.mc"
include "./parse-error.mc"

type CompilationStatus
con UnParsed: String -> CompilationStatus
con Parsed: String -> CompilationStatus
con Keywords: String -> CompilationStatus
con Symbolized: String -> CompilationStatus
con TypeChecked: String -> CompilationStatus
con Compiled: String -> CompilationStatus

let unwrapCompilationStatus: CompilationStatus -> String = lam status.
  switch status
    case UnParsed v then v
    case Parsed v then v
    case Keywords v then v
    case Symbolized v then v
    case TypeChecked v then v
    case Compiled v then v
  end

let pprintCompilationStatus: CompilationStatus -> String = lam status.
  switch status
    case UnParsed err then "UnParsed"
    case Parsed rest then "Parsed"
    case Keywords rest then "Keywords"
    case Symbolized rest then "Symbolized"
    case TypeChecked rest then "TypeChecked"
    case Compiled rest then "Compiled"
  end

let compileFunc: String -> CompilationResult = lam uri.
  -- Heuristic: We compile the program in another process and check the exit code.
  -- This is because the MCore compiler simply crashes if there is a parser error.
  -- We then parse the error message and return it to the client as a diagnostic.

  eprintln (join ["Preparing temporary MCore file for '", uri, "'"]);

  let result = executeCommand (join [
    "/Users/didrik/projects/miking/lsp-demo/mcore-lsp/compile-mcore ",
    uri
  ]) in
  let exitStatus = result.2 in
  let output = strTrim result.1 in

  -- We may encounter errors in all steps.
  -- We are looking for the following checkpoints in the compiler output:
  -- [__PARSED]
  -- [__KEYWORDS]
  -- [__SYMBOLIZED]
  -- [__TYPECHECKED]

  let compilationStatus = if eqi exitStatus 0 then Compiled output else
    match output with "[__PARSED]\n" ++ rest then
      match rest with "[__KEYWORDS]\n" ++ rest then
          match rest with "[__SYMBOLIZED]\n" ++ rest then
              match rest with "[__TYPECHECKED]\n" ++ rest then
                TypeChecked rest
              else Symbolized rest
          else Keywords rest
      else Parsed rest
  else UnParsed output in

  eprintln (join ["Compilation status: ", pprintCompilationStatus compilationStatus]);
  let compilationStatusContent = strTrim (unwrapCompilationStatus compilationStatus) in

  switch compilationStatus
    case Compiled _ then
        let expr = compileFunc { defaultCompileMCoreOptions with debug = true } uri in
        {
          expr = Some expr,
          errors = [],
          warnings = []
        }
    case Symbolized "\nERROR <" ++ _rest then
        let expr = compileFunc { defaultCompileMCoreOptions with debug = true, typeCheck = false } uri in
        let errorMsg = parseMcoreError compilationStatusContent in
        let errorMsg = (
          stripTempFileExtensionFromInfo errorMsg.0,
          errorMsg.1
        ) in
        {
          expr = Some expr,
          errors = [errorMsg],
          warnings = []
        }
    case status then
      switch compilationStatusContent
        case "ERROR <" ++ rest then
          let errorResult = parseMcoreError compilationStatusContent in
          let info = stripTempFileExtensionFromInfo errorResult.0 in
          let msg = errorResult.1 in
          let errorMsg = (info, join ["Compile error (", pprintCompilationStatus status, "): ", msg]) in
          {
            expr = None (),
            errors = [errorMsg],
            warnings = []
          }
        case _ then
          let info = makeInfo {filename = uri, row = 1, col = 0} {filename = uri, row = 1, col = 0} in
          let info = stripTempFileExtensionFromInfo info in
          let errorMsg = (info, join ["Unknown compiler error (", pprintCompilationStatus status, "):", compilationStatusContent]) in
          {
            expr = None (),
            errors = [errorMsg],
            warnings = []
          }
      end
  end

let compileFunc: CompilationParameters -> CompilationResult =
  lam parameters.
    let uri = parameters.uri in
    let content = parameters.content in

    let paths = strSplit "/" (stripUriProtocol uri) in
    let directory = strJoin "/" (init paths) in
    let file = last paths in
    let tmpFilePath = join [directory, "/", file, temp_file_extension] in

    writeFile tmpFilePath content;
    let result = compileFunc tmpFilePath in

    sysDeleteFile tmpFilePath;
    result

mexpr

let environment: LSPEnvironment = {
  files = mapEmpty cmpString
} in

eprintln "Miking MCore LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking MCore LSP ended"

