include "probtime-lib/src/rtppl.mc"
include "mexpr/mexpr.mc"

include "../../lsp-server/lsp/root.mc"
include "../../lsp-server/lsp/utils.mc"

include "./main.mc"
include "./util.mc"

let toDiagnostic = use LSPRoot in lam element. LsDiagnostic element
let toError = use LSPRoot in addSeverity (Error ())
let toWarning = use LSPRoot in addSeverity (Warning ())

lang MExprTypeExtractor = Rtppl + MExprAst + MExpr
  sem extractTypes: Expr -> [(Name, Type)]
  sem extractTypes =
  | expr -> []
  | TmLet { ident = ident, tyAnnot = tyAnnot, tyBody = tyBody } ->
    [(ident, concreteType [tyAnnot, tyBody])]

  sem recursiveExtractTypes: Expr -> [(Name, Type)]
  sem recursiveExtractTypes =| expr ->
    createAccumulators [
      extractTypes,
      createAccumulator sfold_Expr_Expr recursiveExtractTypes
    ] expr
end

lang ProbTimeCompiler =
  MExprTypeExtractor + LSPRoot + Rtppl + MExpr

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
  | TmLam (expr & { ident = ident, parentIdent = None () }) -> 
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

        -- match identRtpplProgram program with
        match program with
          rootProgram & ProgramRtpplProgram program
        in

        let env = initTopEnv rtpplDefaultOptions in
        match mapAccumL compileRtpplTop env program.tops with (topEnv, exprs) in
        
        -- let ast = eliminateDuplicateCode (bind_ runtime rtpplExpr) in
        -- match liftLambdasWithSolutions expr with (solutions, expr) in

        -- BOUNDED EXPR --
        let boundedExpr = bindall_ exprs in
        let prunedBoundedExpr = mexprPruneNonParentIdentifiers boundedExpr in
        -- eprintln (expr2str expr);
        match lsSymbolizeExpr runtimeSymEnv prunedBoundedExpr with {
          expr = symbolizedBoundedExpr
        } in
        -- END BOUNDED EXPR --

        -- FULL EXPR --
        let fullExpr = bind_ runtime boundedExpr in
        match lsSymbolizeExpr runtimeSymEnv fullExpr with {
          expr = symbolizedFullExpr,
          diagnostics = symbolizeDiagnostics
        } in
        -- eprintln (expr2str symbolizedExpr);
        match lsTypeCheckMExpr symbolizedFullExpr with {
          expr = typeCheckedFullExpr,
          diagnostics = typeCheckDiagnostics
        } in
        let types = (compose (mapFromSeq nameCmp) recursiveExtractTypes) typeCheckedFullExpr in
        -- END FULL EXPR --

        let languageSupport = join [
          map toDiagnostic symbolizeDiagnostics,
          map toDiagnostic typeCheckDiagnostics,
          exprToLanguageSupport typeCheckedFullExpr,
          exprToLanguageSupportLinker symbolizedBoundedExpr,
          probtimeProgramToLanguageSupport types rootProgram
        ] in

        eprintln (join ["Compiled ", filename]);
        mapSingleton cmpString filename languageSupport
    end
end