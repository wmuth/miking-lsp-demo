include "javascript/compile.mc"
include "javascript/mcore.mc"
include "mexpr/phase-stats.mc"
include "mexpr/profiling.mc"
include "mexpr/remove-ascription.mc"
include "mexpr/runtime-check.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/utest-generate.mc"
include "mexpr/constant-fold.mc"
include "pmexpr/demote.mc"
include "jvm/compile.mc"
include "mlang/main.mc"
include "peval/compile.mc"

include "file.mc"

lang MCoreCompile =
  BootParser +
  PMExprDemote +
  MExprCmp +
  MExprSym + MExprRemoveTypeAscription + MExprTypeCheck +
  MExprUtestGenerate + MExprRuntimeCheck + MExprProfileInstrument +
  MExprPrettyPrint +
  MExprLowerNestedPatterns +
  MExprConstantFold +
  SpecializeCompile +
  PprintTyAnnot + HtmlAnnotator
end

lang TypeCheckMExpr = MLangPipeline
  type TypeCheckedMExprLSP = {
    expr: Expr,
    warnings: [Diagnostic],
    errors: [Diagnostic]
  }

  sem typeCheckMExprLSP : Expr -> TypeCheckedMExprLSP
  sem typeCheckMExprLSP =| expr ->
    -- Ugly hacking to not make typeCheckExpr
    -- crash in the MLang pipeline
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    let expr = use MCoreCompile in typeCheckExpr {
      typcheckEnvDefault with
      disableConstructorTypes = false
    } expr in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      expr = expr,
      warnings = warnings,
      errors = errors
    }
end

lang MLangCompileMExpr = MLangFileHandler
	sem compileMLang : MLangFile -> MLangFile
	sem compileMLang =
  | file & CSymbolized (symbolized & { program = program }) ->
    use MLangPipeline in
    eprintln (join ["Mexprering file"]);

    match result.consume (checkComposition program) with (warnings, res) in 
    switch res 
      case Left errs then 
        iter raiseError errs ;
        never
      case Right env then
        let ctx = _emptyCompilationContext env in 
        let res = result.consume (compile ctx program) in 
        match res with (_, rhs) in 
        match rhs with Right expr in

        let expr = postprocess env.semSymMap expr in 
        match use TypeCheckMExpr in typeCheckMExprLSP expr with {
          expr = expr,
          warnings = warnings,
          errors = errors
        } in

        CCompiled {
          symbolized = symbolized,
          expr = expr,
          errors = errors,
          warnings = warnings
        }
    end
end