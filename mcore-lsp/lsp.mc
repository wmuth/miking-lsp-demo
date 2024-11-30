include "../miking-lsp/dsl/lsp-server.mc"
include "../../miking/src/main/eval.mc"

include "./compile-mcore.mc"
include "./parse-error.mc"

let compileFunc = lam uri.
  -- Heuristic: We compile the program in another process and check the exit code.
  -- This is because the MCore compiler simply crashes if there is a parser error.
  -- We then parse the error message and return it to the client as a diagnostic.
  let result = executeCommand (join [
    "/Users/didrik/projects/miking/lsp-demo/mcore-lsp/compile-mcore ",
    uri
  ]) in
  let status = result.2 in
  let err = result.1 in

  switch (status, err)
    case (0, _) then compileFunc true uri
    case (1, "ERROR <" ++ rest) then (
      let errorResult = parseMcoreError err in
      let info = stripTempFileExtensionFromInfo errorResult.info in
      let msg = errorResult.msg in

      Left [
        (info, join ["Compile error: ", msg])
      ]
    )
    case (_, _) then (
      let info = makeInfo {filename = uri, row = 1, col = 0} {filename = uri, row = 1, col = 0} in
      let info = stripTempFileExtensionFromInfo info in
      Left [
        (info, join ["Unknown compiler error ", err])
      ]
    )
  end

let compileFunc = lam uri. lam content.
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

eprintln "Miking Probtime LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking Probtime LSP ended"

