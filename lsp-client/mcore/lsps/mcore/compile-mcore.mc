include "../../lsp-server/lsp-server.mc"
include "../../../miking/src/main/eval.mc"

-- let compileFunc =
--   -- : use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations)
--     lam uri. lam content.
--       use ExtMCore in
  
--       eprintln "Parsing Mcore program";
--       eprintln uri;
  
--       let strippedUri = stripUriProtocol uri in
  
--       let ast = parseParseMCoreFile {
--         keepUtests = false,
--         keywords = [],
--         pruneExternalUtests = true,
--         pruneExternalUtestsWarning = true,
--         findExternalsExclude = false, -- the interpreter does not support externals
--         eliminateDeadCode = false
--       } strippedUri in
      
  
--         -- If option --debug-parse, then pretty print the AST
--         -- printLn (mexprToString ast);
  
--       --   let ast = makeKeywords ast in
  
--       --   let ast = symbolize ast in
  
--       --   let ast =
--       --     if options.debugProfile then
--       --       instrumentProfiling ast
--       --     else ast
--       --   in
  
--       --   let ast =
--       --     removeMetaVarExpr
--       --       (typeCheckExpr
--       --         {typcheckEnvDefault with
--       --           disableConstructorTypes = not options.enableConstructorTypes}
--       --         ast)
--       --   in
--       --   (if options.debugTypeCheck then
--       --     printLn (use TyAnnotFull in annotateMExpr ast) else ());
  
--       --   -- If option --test, then generate utest runner calls. Otherwise strip away
--       --   -- all utest nodes from the AST.
--       --   let ast = generateUtest options.runTests ast in
--       --   if options.exitBefore then exit 0
--       --   else
--       --     eval (evalCtxEmpty ()) (updateArgv args ast); ()
--       -- in
--       -- iter evalFile files
  
--       -- use Rtppl in
  
--       -- eprintln "Parsing Mcore program";
--       -- let program = parseRtpplExn uri content in
--       -- eprintln "Parsed Mcore program";
  
--       -- match program with ProgramRtpplProgram p in
  
--       -- match compileRtpplToExpr options p.tops with (llSolutions, topEnv, coreExpr) in
--       -- eprintln "Compiled RTPPL program";
  
--       -- -- let exprs = compileRtpplProgram options program in
--       -- -- let expr = (head (mapToSeq exprs)).1 in
  
  
--       let implementations: LSPImplementations = {
--         hover=[]
--       } in
--       -- let implementations = foldl (
--       --   lam acc. lam x.
--       --     { hover=join [acc.hover, x.hover] }
--       --   ) lsp lspResult
--       -- in
  
--       -- Right (coreExpr, implementations)
  
--       Right (ast, implementations)

type CompileMCoreOptions = {
  debug: Bool,
  printCheckpoints: Bool,
  typeCheck: Bool
}

let defaultCompileMCoreOptions: CompileMCoreOptions = {
  debug = false,
  printCheckpoints = false,
  typeCheck = true
}

-- let compileFunc: use MExprAst in CompileMCoreOptions -> String -> Either [(Info, String)] (Expr, LSPImplementations) =
let compileFunc: use MExprAst in CompileMCoreOptions -> String -> Expr =
  lam options. lam uri.
    use ExtMCore in

    let printCheckpoint = lam value. if options.printCheckpoints then eprintln value; () else () in
    let printDebug = lam value. if options.debug then eprintln value; () else () in

    printDebug (join ["Parsing Mcore program: ", uri]);

    let strippedUri = stripUriProtocol uri in

    let expr = parseParseMCoreFile {
      keepUtests = true,
      keywords = [],
      pruneExternalUtests = true,
      pruneExternalUtestsWarning = true,
      findExternalsExclude = false, -- the interpreter does not support externals
      eliminateDeadCode = false
    } strippedUri in
    printCheckpoint "[__PARSED]";

    printDebug "Making keywords ...";
    let expr = use KeywordMaker in makeKeywords expr in
    printCheckpoint "[__KEYWORDS]";

    printDebug "Symbolizing ...";
    let expr = symbolizeAllowFree expr in
    printCheckpoint "[__SYMBOLIZED]";
    -- eprintln (join ["Symbolized: ", use MExprPrettyPrint in expr2str expr, "\n"]);

    let expr = if options.typeCheck then
      printDebug "Type checking ...";
      let expr = removeMetaVarExpr (typeCheckExpr typcheckEnvDefault expr) in
      printCheckpoint "[__TYPECHECKED]";
      expr
    else expr in

    expr
    
mexpr

-- Compile an MCore program

-- Possible errors:
-- ERROR </Users/didrik/projects/miking/lsp-demo/miking-lsp/test.mc 6:7-6:10>: Unknown variable 'abc'
-- Fatal error: exception Sys_error("/Users/didrik/projects/miking/lsp-demo/miking-lsp/tesat.mc: No such file or directory")

let uri = get argv 1 in

let options = {
  defaultCompileMCoreOptions with
  debug = false,
  printCheckpoints = true
} in

compileFunc options uri;

()