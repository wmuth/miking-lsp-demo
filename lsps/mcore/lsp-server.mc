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
  serverName = "MCore",
  extension = "mc",
  indexWorkspace = false,
  pruneMessages = true,
  printClientMessages = false,
  benchmark = true
} in

let onOpen = use MLangCompiler in createFileLoader (Open ()) in
let onChange = use MLangCompiler in createFileLoader (Change ()) in

let lspStartParameters: use LSPRoot in LSPStartParameters = {
  onOpen   = onOpen,
  onChange = onChange,
  onClose  = onClose,
  options  = options
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"