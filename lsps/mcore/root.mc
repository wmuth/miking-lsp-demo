include "mlang/main.mc"
include "../../lsp-server/lsp/root.mc"

lang MLangRoot = MLangPipeline + LanguageServer
  type Path = String

  type MLangFile = {
    content: String,
    filename: Path,
    program: Option MLangProgram,
    symEnv: Option SymEnv,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }
end

let emptyMLangFile: use MLangRoot in MLangFile = {
  content = "",
  filename = "",
  program = None (),
  symEnv = Some _symEnvEmpty,
  errors = [],
  warnings = []
}