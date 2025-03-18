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

include "./root.mc"

lang MCoreCompile =
  BootParser +
  PMExprDemote +
  MExprCmp +
  MExprSym + MExprRemoveTypeAscription + MExprTypeCheck +
  MExprUtestGenerate + MExprRuntimeCheck + MExprProfileInstrument +
  MLangPrettyPrint +
  MExprLowerNestedPatterns +
  MExprConstantFold +
  SpecializeCompile +
  PprintTyAnnot + HtmlAnnotator
end

let typeCheckBuffer: Ref TCEnv = ref typcheckEnvDefault

lang LSPTypeCheck = MCoreCompile
  syn Expr =
  | TmBufferTypeCheckForLSP { inexpr: Expr }

  sem removeMetaVarExpr =
  | TmBufferTypeCheckForLSP { inexpr=inexpr } ->
    removeMetaVarExpr inexpr

  sem typeCheckExpr env =
  | TmUse { inexpr=inexpr } ->
    typeCheckExpr env inexpr
  | TmBufferTypeCheckForLSP { inexpr=inexpr } ->
    modref typeCheckBuffer env;
    typeCheckExpr env inexpr
end

lang MLangMExprTypeChecker = MLangRoot
  type TypeCheckedMExprLSP = {
    expr: Expr,
    tcEnv: TCEnv,
    warnings: [Diagnostic],
    errors: [Diagnostic]
  }

  sem lsTypeCheckMExpr : TCEnv -> Expr -> TypeCheckedMExprLSP
  sem lsTypeCheckMExpr env =| expr ->
    -- Ugly hacking to not make typeCheckExpr
    -- crash in the MLang pipeline
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    let env = {
      env with
      disableConstructorTypes = true
    } in

    use LSPTypeCheck in
    let bufferExpr = TmBufferTypeCheckForLSP { inexpr = uunit_ } in
    let expr = bind_ expr bufferExpr in

    -- let expr = use LSPTypeCheck in removeMetaVarExpr (typeCheckExpr env expr) in
    modref typeCheckBuffer typcheckEnvDefault;
    let expr = removeMetaVarExpr (typeCheckExpr env expr) in
    let tcEnv = deref typeCheckBuffer in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      expr = expr,
      tcEnv = tcEnv,
      warnings = warnings,
      errors = errors
    }
end

lang MLangMExprCompiler = MLangRoot
	sem lsCompileMLangToMExpr : (Path -> Option MLangFile) -> Path -> [Link] -> Option MLangProgram -> MLangTypeCheckedFile
	sem lsCompileMLangToMExpr getFile filename includes =
  | None () -> {
    expr = None (),
    tcEnv = typcheckEnvEmpty,
    compositionEnv = _emptyCompositionCheckEnv,
    diagnostics = []
  }
  | Some program ->
    let includedFiles: [MLangFile] = filterMap (lam link. getFile link.1) includes in
    
    let typCheckedPrograms = filterMap (lam file. file.typeChecked) includedFiles in
    let typCheckedEnvs = map (lam typeChecked. typeChecked.tcEnv) typCheckedPrograms in
    let tcEnv = foldl mergeTypcheckEnv typcheckEnvDefault typCheckedEnvs in

    let typCheckedCompositionEnvs = map (lam typeChecked. typeChecked.compositionEnv) typCheckedPrograms in

    match result.consume (checkComposition program) with (warnings, res) in 
    switch res 
      case Left errs then
        {
          expr = None (),
          tcEnv = typcheckEnvEmpty,
          compositionEnv = _emptyCompositionCheckEnv,
          diagnostics = map (compose (addSeverity (Error ())) getCompositionErrorDiagnostic) errs
        }
      case Right compositionEnv then
        -- let compositionEnv = foldl mergeCompositionCheckEnv compositionEnv typCheckedCompositionEnvs in
        let ctx = _emptyCompilationContext compositionEnv in        

        let declCtx = result.foldlM compileDecl ctx program.decls in
        match result.consume declCtx with (_, Right ctx) in

        let ctx = withExpr ctx program.expr in
        let expr = bindall_ ctx.exprs in
        let expr = postprocess compositionEnv.semSymMap expr in

        match use MLangMExprTypeChecker in lsTypeCheckMExpr tcEnv expr with {
          expr = expr,
          tcEnv = tcEnv,
          warnings = warnings,
          errors = errors
        } in

        {
          expr = Some expr,
          tcEnv = tcEnv,
          compositionEnv = compositionEnv,
          diagnostics = join [
            map (addSeverity (Error ())) errors,
            map (addSeverity (Warning ())) warnings
          ]
        }
    end
end