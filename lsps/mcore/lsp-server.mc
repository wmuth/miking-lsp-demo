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
  extension = "mc",
  indexWorkspace = false,
  pruneMessages = true
} in

let fileLoader = use MLangCompiler in createFileLoader () in

let lspStartParameters: use LSPRoot in LSPStartParameters = {
  onOpen   = fileLoader,
  onChange = fileLoader,
  onClose  = onClose,
  options  = options
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"