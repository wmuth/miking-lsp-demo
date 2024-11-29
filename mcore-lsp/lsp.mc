include "../miking-lsp/dsl/lsp-server.mc"
include "./compile-mcore.mc"
include "../../miking/src/main/eval.mc"

let parseMcoreError = lam err.
  -- ERROR </Users/didrik/projects/miking/lsp-demo/miking-lsp/test.mc 6:7-6:10>: Unknown variable 'abc'

  let parsePos = lam uri. lam pos.
    let parts = strSplit ":" pos in
    let row = string2int (head parts) in
    let col = string2int (head (tail parts)) in

    {filename = uri, row = row, col = col}
  in

  let parseLocation = lam location.
    let locationParts = strSplit " " location in
    let uri = head locationParts in
    let positions = strSplit "-" (join (tail locationParts)) in
    let startPos = parsePos uri (head positions) in
    let endPos = parsePos uri (head (tail positions)) in

    makeInfo startPos endPos
  in

  match err with "ERROR <" ++ rest then
    let parts = strSplit ">: " rest in
    let locationInfo = parseLocation (head parts) in
    let msg = join (tail parts) in

    {info = locationInfo, msg = msg}

  else error "Invalid error format in `parseMcoreError`"

let compileFunc = lam uri. lam content.
  let result = executeCommand (join [
    "/Users/didrik/projects/miking/lsp-demo/mcore-lsp/compile-mcore ",
    uri
  ]) in
  let status = result.2 in
  let err = result.1 in

  if eqi status 1 then (
    match err with "ERROR <" ++ rest then
      -- ERROR </Users/didrik/projects/miking/lsp-demo/miking-lsp/test.mc 6:7-6:10>: Unknown variable 'abc'

      let errorResult = parseMcoreError err in
      let info = errorResult.info in
      let msg = errorResult.msg in

      Left [
        (info, join ["Compile error: ", msg])
      ]
    else
      let info = makeInfo {filename = uri, row = 1, col = 1} {filename = uri, row = 1, col = 100} in
      Left [
        (info, join ["Unknown compiler error ", err])
      ]
  ) else
  
  compileFunc true uri content

mexpr

let environment: LSPEnvironment = {
  files = mapEmpty cmpString
} in

eprintln "Miking Probtime LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking Probtime LSP ended"

