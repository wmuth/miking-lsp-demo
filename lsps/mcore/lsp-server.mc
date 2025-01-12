include "mlang/main.mc"
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
  pruneMessages = true
} in

let lspStartParameters: use LSPRoot in LSPStartParameters = {
  onOpen   = use MLangCompiler in compileMLangLSP config,
  onChange = use MLangCompiler in compileMLangLSP config,
  onClose  = onClose,
  options  = options
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"