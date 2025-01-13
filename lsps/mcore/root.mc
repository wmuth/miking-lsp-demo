include "mlang/main.mc"
include "../../lsp-server/lsp/root.mc"

lang MLangRoot = MLangPipeline + LanguageServer
  type Path = String

  type MLangFile = {
    content: String,
    filename: Path,
    program: Option MLangProgram,
    includes: [(Info, Path)], -- Todo: iterate on this type
    symEnv: Option SymEnv,
    errors: [Diagnostic],
    warnings: [Diagnostic]
  }
end

let emptyMLangFile: use MLangRoot in MLangFile = {
  content = "",
  filename = "",
  program = None (),
  includes = [],
  symEnv = Some _symEnvEmpty,
  errors = [],
  warnings = []
}