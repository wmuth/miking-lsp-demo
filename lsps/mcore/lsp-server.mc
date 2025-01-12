include "mlang/main.mc"
include "../../lsp-server/lsp-server.mc"

include "./compiler.mc"

let onClose: Ref MLangEnvironment -> String -> () =
  lam mLangEnvironment. lam uri.
    let dereffed = deref mLangEnvironment in
    let newMLangEnvironment = {
      dereffed with
      files = mapRemove uri dereffed.files
    } in
    modref mLangEnvironment newMLangEnvironment

mexpr

let config = defaultLSPOptions.config in
let options = {
  defaultLSPOptions with
  config = config,
  pruneMessages = true
} in

let mLangEnvironment = {
  files = mapEmpty cmpString,
  dependencies = mapEmpty cmpString
} in

let environment = ref mLangEnvironment in

let lspStartParameters: use LSPRoot in LSPStartParameters = {
  onOpen   = use MLangCompiler in compile config environment,
  onChange = use MLangCompiler in compile config environment,
  onClose  = onClose environment,
  options  = options
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"