include "../../lsp-server/lsp-server.mc"
include "./compiler.mc"

let onClose: String -> () =
  lam path.
    eprintln (join ["Closing file ", path])

mexpr

let options = {
  defaultLSPOptions with
  config = defaultLSPOptions.config,
  serverName = "ProbTime",
  extension = "rpl",
  indexWorkspace = false,
  pruneMessages = true,
  printClientMessages = false,
  benchmark = true,
  
  filterHoverDuplicates = true,
  hoverShowDefinitionPrefix = Some identity
} in

let changeHandler = use ProbTimeCompiler in createChangeHandler () in

let lspStartParameters: use LSPRoot in LSPStartParameters = {
  onOpen   = changeHandler,
  onChange = changeHandler,
  onClose  = onClose,
  options  = options
} in

eprintln "Miking ProbTime LSP started";
startLSPServer lspStartParameters;
eprintln "Miking ProbTime LSP ended"