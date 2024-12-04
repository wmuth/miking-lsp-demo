include "../../lsp-server/lsp-server.mc"
include "../../dsls/didrik/dsl.mc"

let compileFunc: CompilationParameters -> CompilationResult =
  lam parameters.
    let uri = parameters.uri in
    let strippedUri = stripUriProtocol uri in
    let content = parameters.content in

    use Complete in
    switch parseCalc strippedUri content
      case Right file then
        let expr = compileStatementsToMexpr (fileToStatements file) in
        let expr = use KeywordMaker in makeKeywords expr in
        let expr = use MExprSym in symbolizeAllowFree expr in
        let expr = use MExprTypeCheck in removeMetaVarExpr (typeCheckExpr typcheckEnvDefault expr) in
        -- eprintln (use MExprPrettyPrint in expr2str expr);
        {
          expr = Some expr,
          errors = [],
          warnings = []
        }
      case Left errors then
        {
          expr = None (),
          errors = errors,
          warnings = []
        }
    end

mexpr

let environment: LSPEnvironment = {
  files = mapEmpty cmpString
} in

eprintln "Miking Didrik LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking Didrik LSP ended"

