include "probtime-lib/src/rtppl.mc"
include "mexpr/mexpr.mc"

include "../../lsp-server/lsp/root.mc"
include "../../lsp-server/lsp/utils.mc"

include "./main.mc"

let toDiagnostic = use LSPRoot in lam element. LsDiagnostic element
let toError = use LSPRoot in addSeverity (Error ())
let toWarning = use LSPRoot in addSeverity (Warning ())

lang ProbTimeCompiler =
  LSPRoot + Rtppl + MExpr

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

  sem mexprPruneNonParentIdentifier: Expr -> Expr
  sem mexprPruneNonParentIdentifier =
  | expr -> expr
  | TmLam (expr & { ident = ident }) -> 
    TmLam {
      expr with
      ident = nameNoSym ""
    }
  | TmLet (expr & { parentIdent = None () }) ->
    TmLet {
      expr with
      ident = nameNoSym ""
    }

  sem mexprPruneNonParentIdentifiers: Expr -> Expr
  sem mexprPruneNonParentIdentifiers =| expr ->
    let expr = mexprPruneNonParentIdentifier expr in
    smap_Expr_Expr mexprPruneNonParentIdentifiers expr

  sem createExprLanguageSupport: String -> SymEnv -> Expr -> [LanguageServerPayload]
  sem createExprLanguageSupport filename symEnv =| expr ->
    match lsSymbolizeExpr symEnv expr with {
      expr = symbolizedExpr,
      diagnostics = symbolizeDiagnostics
    } in

    match lsTypeCheckMExpr symbolizedExpr with {
      expr = typeCheckedExpr,
      diagnostics = typeCheckDiagnostics
    } in

    let languageSupport = exprToLanguageSupport filename typeCheckedExpr in
    
    join [
      map toDiagnostic symbolizeDiagnostics,
      map toDiagnostic typeCheckDiagnostics,
      languageSupport
    ]

  sem createExprLinkerLanguageSupport: String -> SymEnv -> Expr -> [LanguageServerPayload]
  sem createExprLinkerLanguageSupport filename symEnv =| expr ->
    let expr = mexprPruneNonParentIdentifiers expr in
    eprintln (expr2str expr);

    match lsSymbolizeExpr symEnv expr with {
      expr = symbolizedExpr,
      diagnostics = symbolizeDiagnostics
    } in

    let languageSupport = exprToLanguageSupportLinker filename symbolizedExpr in
    languageSupport

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
      case Right program then  
        eprintln (join ["Parsed ", filename]);
        -- validateRtpplProgram program;

        match identRtpplProgram program with
          rootProgram & ProgramRtpplProgram program
        in

        let env = initTopEnv rtpplDefaultOptions in
        match mapAccumL compileRtpplTop env program.tops with 
        (
          topEnv,
          exprs
        ) in

        let boundedExpr = bindall_ exprs in
        let fullExpr = bind_ runtime boundedExpr in
        -- eprintln (expr2str boundedExpr);
        
        -- let ast = eliminateDuplicateCode (bind_ runtime rtpplExpr) in
        -- match liftLambdasWithSolutions expr with (
        --   solutions,
        --   expr
        -- ) in

        let languageSupport = join [
          createExprLanguageSupport filename runtimeSymEnv fullExpr,
          createExprLinkerLanguageSupport filename runtimeSymEnv boundedExpr,
          probtimeProgramToLanguageSupport filename rootProgram
        ] in

        eprintln (join ["Compiled ", filename]);
        mapSingleton cmpString filename languageSupport
    end
end