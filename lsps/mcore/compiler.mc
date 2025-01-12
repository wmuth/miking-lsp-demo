include "mlang/main.mc"

include "./file.mc"
include "./utests.mc"
include "./main.mc"
include "./upgrade.mc"

include "../../lib/utils.mc"
include "../../lsp-server/lsp/root.mc"

lang MLangCompiler =
  LanguageServer +
  MLangAst + MExprAst +
  MLangIncludeHandler +
  MLangFileHandler + MLangUpgradeFile

  sem compile: LSPConfig -> CompilationParameters -> CompilationResult
  sem compile config =| parameters ->
    let uri = stripUriProtocol parameters.uri in
    let content = parameters.content in

    let file = upgradeFile uri (CLoaded { content = content, filename = uri }) in

    join [
      use MLangLookupVariable in fileToLanguageSupport file,
      map (lam diagnostic. LsDiagnostic { location=diagnostic.0, message=diagnostic.1, severity=Error () }) (getFileErrors file),
      map (lam diagnostic. LsDiagnostic { location=diagnostic.0, message=diagnostic.1, severity=Warning () }) (getFileWarnings file)
    ]
end