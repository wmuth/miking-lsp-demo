include "../../lsp-demo/miking-lsp/dsl/lsp-server.mc"
include "rtppl.mc"


mexpr

let options = {
  debugParse = false,
  debugCompileDppl = false,
  debugCompileMExpr = false,
  outputPath = "",
  bufferSize = slli 1 22,
  file = ""
} in

-- let program = parseRtpplExn options.file content in
-- validateRtpplProgram program;
-- let result = compileRtpplProgram options program in

let compileFunc: use MExprAst in String -> String -> Either [(Info, String)] (Expr, LSPImplementations) =
	lam uri. lam content.
    use Rtppl in

    eprintln "Parsing RTPPL program";
    let program = parseRtpplExn uri content in
    eprintln "Parsed RTPPL program";

    match program with ProgramRtpplProgram p in

    match compileRtpplToExpr options p.tops with (llSolutions, topEnv, coreExpr) in
    eprintln "Compiled RTPPL program";

    -- let exprs = compileRtpplProgram options program in
    -- let expr = (head (mapToSeq exprs)).1 in


    let implementations: LSPImplementations = {
      hover=[]
    } in
    -- let implementations = foldl (
    --   lam acc. lam x.
    --     { hover=join [acc.hover, x.hover] }
    --   ) lsp lspResult
    -- in

    Right (coreExpr, implementations)
  in

let environment: LSPEnvironment = {
  findVariable = lam. lam. lam. None (),
  findDefinition = lam. None ()
} in

eprintln "Miking Probtime LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking Probtime LSP ended"