include "probtime-lib/src/rtppl.mc"
include "mexpr/mexpr.mc"

include "../../lsp-server/lsp/root.mc"
include "../../lsp-server/lsp/utils.mc"

include "./main.mc"

let toDiagnostic = use LSPRoot in lam element. LsDiagnostic element
let toError = use LSPRoot in addSeverity (Error ())
let toWarning = use LSPRoot in addSeverity (Warning ())

lang ProbTimeCompiler = LSPRoot + Rtppl + MExpr + MExprLanguageServerCompiler
  type MExprSymbolized = {
    expr: Expr, -- With symbols
    diagnostics: [DiagnosticWithSeverity]
  }

  type MExprTypeChecked = {
    expr: Expr,
    diagnostics: [DiagnosticWithSeverity]
  }

  sem lsSymbolizeExpr: SymEnv -> Expr -> MExprSymbolized
  sem lsSymbolizeExpr symEnv =| expr ->
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    let expr = symbolizeExpr symEnv expr in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      expr = expr,
      diagnostics = join [
        map toError errors,
        map toWarning warnings
      ]
    }
  
  sem lsTypeCheckMExpr : Expr -> MExprTypeChecked
  sem lsTypeCheckMExpr =| expr ->
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    let env = {
      typcheckEnvDefault with
      disableConstructorTypes = true
    } in

    let expr = removeMetaVarExpr (typeCheckExpr env expr) in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      expr = expr,
      diagnostics = join [
        map toError errors,
        map toWarning warnings
      ]
    }

  sem createChangeHandler: () -> (LSPCompilationParameters -> LSPCompilationResult)
  sem createChangeHandler =| _ ->
    let runtime = readRuntime () in
    let runtimeSymEnv = addTopNames symEnvEmpty runtime in
    onChange runtime runtimeSymEnv

  sem onChange: Expr -> SymEnv -> (LSPCompilationParameters -> LSPCompilationResult)
  sem onChange runtime runtimeSymEnv =| parameters ->
    let filename = stripUriProtocol parameters.uri in

    switch parseRtppl filename parameters.content
      case Left errors then
        eprintln (join ["Error parsing ", filename]);
        let diagnostics = map (compose toDiagnostic toError) errors in
        mapSingleton cmpString filename diagnostics
      case Right (ProgramRtpplProgram program) then  
        eprintln (join ["Parsed ", filename]);
        -- validateRtpplProgram program;

        let env = initTopEnv rtpplDefaultOptions in
        match mapAccumL compileRtpplTop env program.tops with 
        (
          topEnv,
          exprs
        ) in

        let expr = bindall_ exprs in
        let expr = bind_ runtime expr in

        match lsSymbolizeExpr runtimeSymEnv expr with {
          expr = expr,
          diagnostics = symbolizeDiagnostics
        } in

        match lsTypeCheckMExpr expr with {
          expr = expr,
          diagnostics = typeCheckDiagnostics
        } in
        
        -- let ast = eliminateDuplicateCode (bind_ runtime rtpplExpr) in

        -- match liftLambdasWithSolutions expr with (
        --   solutions,
        --   expr
        -- ) in

        let languageSupport = fileToLanguageSupport filename expr in

        eprintln (join ["Compiled ", filename]);
        mapSingleton cmpString filename (join [
          map toDiagnostic symbolizeDiagnostics,
          map toDiagnostic typeCheckDiagnostics,
          languageSupport
        ])
    end
end