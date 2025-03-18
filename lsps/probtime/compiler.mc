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
  | TmLet { ident = ident, tyAnnot = tyAnnot, tyBody = tyBody }
  | TmLam { ident = ident, tyAnnot = tyAnnot, tyParam = tyBody } ->
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

  type RtpplTopResult = {
    env: RtpplTopEnv,
    exprs: [Expr],
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

  sem lsCompileRtpplTop: [RtpplTop] -> RtpplTopResult
  sem lsCompileRtpplTop =| tops ->
    modref __LSP__SOFT_ERROR true;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    let env = initTopEnv rtpplDefaultOptions in
    match mapAccumL compileRtpplTop env tops with (topEnv, exprs) in

    let errors = deref __LSP__BUFFERED_ERRORS in
    let warnings = deref __LSP__BUFFERED_WARNINGS in

    modref __LSP__SOFT_ERROR false;
    modref __LSP__BUFFERED_ERRORS [];
    modref __LSP__BUFFERED_WARNINGS [];

    {
      env = topEnv,
      exprs = exprs,
      diagnostics = join [
        map toError errors,
        map toWarning warnings
      ]
    }

  -- In order to properly link according to the native ProbTime
  -- constructs, we remove all identifiers that are not symbolized.
  -- The ProbTime .syn file will use UNameSym for types which are
  -- expected to define identifiers. The UNameSym will use nameSym
  -- to generate a symbol for the identifier automatically.
  sem mexprPruneNonSymbolizedIdentifier: Expr -> Expr
  sem mexprPruneNonSymbolizedIdentifier =
  | expr -> expr
  | e & TmLam (expr & { ident = ident }) ->
    if nameHasSym ident then e else
    TmLam {
      expr with
      ident = nameNoSym ""
    }
  | e & TmLet (expr & { ident = ident }) ->
    if nameHasSym ident then e else
    TmLet {
      expr with
      ident = nameNoSym ""
    }

  sem mexprPruneNonSymbolizedIdentifiers: Expr -> Expr
  sem mexprPruneNonSymbolizedIdentifiers =| expr ->
    let expr = mexprPruneNonSymbolizedIdentifier expr in
    smap_Expr_Expr mexprPruneNonSymbolizedIdentifiers expr

--   sem createStdlib: () -> (RtpplProgram, Expr)
--   sem createStdlib =| _ ->
--       let program = "
-- // Hejsan
-- def sqrt(v: Float): Float {}

-- system {}
--       " in
--       match parseRtppl "stdlib" program
--       with Right (rootProgram, ProgramRtpplProgram program) then
--         match lsCompileRtpplTop program.tops
--         with { exprs = exprs } in
--         (rootProgram, bindall_ exprs)
--       else
--         error "Failed to create stdlib"

  sem createChangeHandler: () -> (LSPCompilationParameters -> LSPCompilationResult)
  sem createChangeHandler =| _ ->
    let runtime = readRuntime () in
    let runtimeSymEnv = addTopNames symEnvEmpty runtime in
    onChange runtime runtimeSymEnv

  sem onChange: Expr -> SymEnv -> (LSPCompilationParameters -> LSPCompilationResult)
  sem onChange runtime runtimeSymEnv =| parameters ->
    let filename = stripUriProtocol parameters.uri in

    -- match createStdlib () with (stdlib, stdlibExpr) in

    switch parseRtppl filename parameters.content
      case Left errors then
        eprintln (join ["Error parsing ", filename]);
        let diagnostics = map (compose toDiagnostic toError) errors in
        mapSingleton cmpString filename diagnostics
      case Right (rootProgram & ProgramRtpplProgram program) then  
        eprintln (join ["Parsed ", filename]);
        -- validateRtpplProgram program;

        match lsCompileRtpplTop program.tops with {
          exprs = exprs,
          diagnostics = compileDiagnostics
        } in
        
        -- let ast = eliminateDuplicateCode (bind_ runtime rtpplExpr) in
        -- match liftLambdasWithSolutions expr with (solutions, expr) in

        -- BOUNDED EXPR --
        let boundedExpr = bindall_ exprs in
        -- let boundedExprStdlib = bind_ stdlibExpr boundedExpr in
        -- eprintln (expr2str boundedExprStdlib);

        let prunedBoundedExpr = mexprPruneNonSymbolizedIdentifiers boundedExpr in
        -- let prunedBoundedExpr = mexprPruneNonSymbolizedIdentifiers boundedExprStdlib in
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

        let t1 = wallTimeMs () in

        let types = (compose (mapFromSeq nameCmp) recursiveExtractTypes) typeCheckedFullExpr in

        -- eprintln (join [
        --   "Types: \t",
        --   strJoin "\n\t" (map (lam typ. match typ with (name, typ) in
        --     join [
        --       nameGetStr name,
        --       ": ", type2str typ,
        --       "(", getSym name, ")"
        --     ]
        --   ) (mapToSeq types))
        -- ]);

        -- END FULL EXPR --

        let languageSupport = join [
          map toDiagnostic compileDiagnostics,
          map toDiagnostic symbolizeDiagnostics,
          map toDiagnostic typeCheckDiagnostics,
          -- exprToLanguageSupport typeCheckedFullExpr,
          exprToLanguageSupportLinker types symbolizedBoundedExpr,
          -- probtimeProgramToLanguageSupport types stdlib,
          probtimeProgramToLanguageSupport types rootProgram
        ] in

        let t2 = wallTimeMs () in
        let time = subf t2 t1 in
        eprintln (join ["Time taken (ProbTime onChange): ", float2string time, "ms"]);

        eprintln (join ["Compiled ", filename]);
        mapSingleton cmpString filename languageSupport
    end
end