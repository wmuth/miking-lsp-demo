include "../../lsp-server/lsp-server.mc"

include "./compiler.mc"

let onClose: String -> () =
  lam path.
    eprintln (join ["Closing file ", path])

mexpr

let config = defaultLSPOptions.config in
let options = {
  defaultLSPOptions with
  config = config,
  serverName = "ProbTime",
  extension = "rpl",
  indexWorkspace = false,
  pruneMessages = true,
  printClientMessages = true
} in

let lspStartParameters: use LSPRoot in LSPStartParameters = {
  onOpen   = use ProbTimeCompiler in createChangeHandler (),
  onChange = use ProbTimeCompiler in createChangeHandler (),
  onClose  = onClose,
  options  = options
} in

eprintln "Miking ProbTime LSP started";
startLSPServer lspStartParameters;
eprintln "Miking ProbTime LSP ended"