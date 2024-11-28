include "../miking-lsp/dsl/lsp-server.mc"
include "../../miking/src/main/eval.mc"
-- include "rtppl.mc"

mexpr

let compileFunc =
-- : use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations)
	lam uri. lam content.
    use ExtMCore in

    eprintln "Parsing Mcore program";
    eprintln uri;

    let strippedUri = stripUriProtocol uri in

    let ast = parseParseMCoreFile {
      keepUtests = false,
      keywords = [],
      pruneExternalUtests = true,
      pruneExternalUtestsWarning = true,
      findExternalsExclude = false, -- the interpreter does not support externals
      eliminateDeadCode = false
    } strippedUri in

      -- If option --debug-parse, then pretty print the AST
      -- printLn (mexprToString ast);

    --   let ast = makeKeywords ast in

    --   let ast = symbolize ast in

    --   let ast =
    --     if options.debugProfile then
    --       instrumentProfiling ast
    --     else ast
    --   in

    --   let ast =
    --     removeMetaVarExpr
    --       (typeCheckExpr
    --         {typcheckEnvDefault with
    --           disableConstructorTypes = not options.enableConstructorTypes}
    --         ast)
    --   in
    --   (if options.debugTypeCheck then
    --     printLn (use TyAnnotFull in annotateMExpr ast) else ());

    --   -- If option --test, then generate utest runner calls. Otherwise strip away
    --   -- all utest nodes from the AST.
    --   let ast = generateUtest options.runTests ast in
    --   if options.exitBefore then exit 0
    --   else
    --     eval (evalCtxEmpty ()) (updateArgv args ast); ()
    -- in
    -- iter evalFile files

    -- use Rtppl in

    -- eprintln "Parsing Mcore program";
    -- let program = parseRtpplExn uri content in
    -- eprintln "Parsed Mcore program";

    -- match program with ProgramRtpplProgram p in

    -- match compileRtpplToExpr options p.tops with (llSolutions, topEnv, coreExpr) in
    -- eprintln "Compiled RTPPL program";

    -- -- let exprs = compileRtpplProgram options program in
    -- -- let expr = (head (mapToSeq exprs)).1 in


    let implementations: LSPImplementations = {
      hover=[]
    } in
    -- let implementations = foldl (
    --   lam acc. lam x.
    --     { hover=join [acc.hover, x.hover] }
    --   ) lsp lspResult
    -- in

    -- Right (coreExpr, implementations)

    Right (ast, implementations)
in

-- compileFunc "./mcore-lsp/lsp.mc" "";

-- ()

let environment: LSPEnvironment = {
  files = mapEmpty cmpString
} in

eprintln "Miking Probtime LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking Probtime LSP ended"

