-- include "mlang/boot-parser.mc"
include "mlang/main.mc"

include "../../lsp-server/lsp-server.mc"
include "../../../miking/src/main/eval.mc"

include "./compile-mcore.mc"
include "./parse-error.mc"

type CompilationStatus
con Unparsed: String -> CompilationStatus
con Parsed: String -> CompilationStatus
con Keywords: String -> CompilationStatus
con Symbolized: String -> CompilationStatus
con TypeChecked: String -> CompilationStatus
con Compiled: String -> CompilationStatus

let unwrapCompilationStatus: CompilationStatus -> String = lam status.
  switch status
    case Unparsed v then v
    case Parsed v then v
    case Keywords v then v
    case Symbolized v then v
    case TypeChecked v then v
    case Compiled v then v
  end

let pprintCompilationStatus: CompilationStatus -> String = lam status.
  switch status
    case Unparsed err then "Unparsed"
    case Parsed rest then "Parsed"
    case Keywords rest then "Keywords"
    case Symbolized rest then "Symbolized"
    case TypeChecked rest then "TypeChecked"
    case Compiled rest then "Compiled"
  end

let compileFunc: CompilationParameters -> String -> CompilationResult =
  lam parameters. lam uri.
    -- Heuristic: We compile the program in another process and check the exit code.
    -- This is because the MCore compiler simply crashes if there is a parser error.
    -- We then parse the error message and return it to the client as a diagnostic.

    eprintln "Compiling first round of MCore file";
    -- let cmd = join ["echo $(PWD) >>/dev/stderr"] in eprintln output;
    let cmd = join ["./lsps/mcore/compile-mcore ", uri] in
    let result = executeCommand cmd in
    let exitStatus = result.2 in
    let output = strTrim result.1 in

    eprintln output;

    -- We may encounter errors in all steps.
    -- We are looking for the following checkpoints in the compiler output:
    -- [__PARSED]
    -- [__KEYWORDS]
    -- [__SYMBOLIZED]
    -- [__TYPECHECKED]

    let compilationStatus = if eqi exitStatus 0 then Compiled output else
      match output with "[__PARSED]\n" ++ rest then
        match rest with "[__KEYWORDS]\n" ++ rest then
            match rest with "[__SYMBOLIZED]\n" ++ rest then
                match rest with "[__TYPECHECKED]\n" ++ rest then
                  TypeChecked rest
                else Symbolized rest
            else Keywords rest
        else Parsed rest
    else Unparsed output in

    eprintln (join ["... compilation status: ", pprintCompilationStatus compilationStatus]);
    eprintln "Compiling second round of MCore file";
    let compilationStatusContent = strTrim (unwrapCompilationStatus compilationStatus) in

    switch compilationStatus
      case Compiled _ then
        -- Notify the client about the partial result,
        -- we at least don't have errors
        parameters.notifyPartialResult {
          expr = None (),
          errors = [],
          warnings = []
        };

        let expr = compileFunc { defaultCompileMCoreOptions with debug = true } uri in
        {
          expr = Some expr,
          errors = [],
          warnings = []
        }
      case Symbolized "\nERROR <" ++ _rest then
        let expr = compileFunc { defaultCompileMCoreOptions with debug = true, typeCheck = false } uri in
        let errorMsg = parseMcoreError compilationStatusContent in
        let errorMsg = (
          stripTempFileExtensionFromInfo errorMsg.0,
          strTrim errorMsg.1
        ) in
        {
          expr = Some expr,
          errors = [errorMsg],
          warnings = []
        }
      case status then
        switch compilationStatusContent
          case "ERROR <" ++ rest then
            let errorResult = parseMcoreError compilationStatusContent in
            let info = stripTempFileExtensionFromInfo errorResult.0 in
            let msg = strTrim errorResult.1 in
            let errorMsg = (info, join ["Compile error (", pprintCompilationStatus status, "): ", msg]) in
            {
              expr = None (),
              errors = [errorMsg],
              warnings = []
            }
          case _ then
            let info = makeInfo {filename = uri, row = 1, col = 0} {filename = uri, row = 1, col = 0} in
            let info = stripTempFileExtensionFromInfo info in
            let errorMsg = (info, join ["Unknown compiler error (", pprintCompilationStatus status, "):", compilationStatusContent]) in
            {
              expr = None (),
              errors = [errorMsg],
              warnings = []
            }
        end
    end



let compileFunc: CompilationParameters -> CompilationResult =
  lam parameters.
    let uri = parameters.uri in
    let content = parameters.content in

    -- use BootParserMLang in
    use MLangPipeline in
    -- match result.consume (parseMLangString str) with (_, Right p) in

    -- match result.consume (parseMLangFile path) with (_, errOrProg) in
    -- switch errOrProg
    --   case Left err then error (join [
    --     "File '",
    --     path,
    --     "' could not be parsed!"
    --   ])
    --   case Right prog then 
    --     -- modref included (setInsert path s);
    --     -- handleIncludesProgram included dir libs prog
    -- end

    -- let p = parseMLangString content in
    match result.consume (parseMLangString content) with (_, Right p) in
    
    let p = constTransformProgram builtin p in
    let p = composeProgram p in
    match symbolizeMLang symEnvDefault p with (_, p) in
    match result.consume (checkComposition p) with (_, res) in
    (switch res
      case Right env then
        let ctx = _emptyCompilationContext env in 
        let res = result.consume (compile ctx p) in 
        match res with (_, rhs) in 
        match rhs with Right expr in
        printLn (expr2str expr);
        ()
    end);

    -- let p = parseAndHandleIncludes (stripUriProtocol uri) in 
    -- let p = constTransformProgram builtin p in
    -- let p = composeProgram p in 
    -- match symbolizeMLang symEnvDefault p with (_, p) in 
    -- match result.consume (checkComposition p) with (_, res) in 

    -- switch res 
    --   case Left errs then 
    --     iter raiseError errs ;
    --     never
    --   case Right env then
    --     let ctx = _emptyCompilationContext env in 
    --     let res = result.consume (compile ctx p) in 
    --     match res with (_, rhs) in 
    --     match rhs with Right expr in
    --     endPhaseStats log "mlang-mexpr-lower" expr;

    --     let expr = postprocess env.semSymMap expr in 
    --     endPhaseStats log "postprocess" expr;

    --     -- printLn (expr2str expr);

    --     runner options filepath expr;
    --     ()
    -- end

    {
      expr = None (),
      errors = [],
      warnings = []
    }



    -- (
    --   use BootParserMLang in
    --   let parseProgram = lam str.
    --     match result.consume (parseMLangString str) with (_, Right p) in p
    --   in

    --   let getIncludeStrings : MLangProgram -> [String] = lam p.
    --     let decls = p.decls in
    --     mapOption
    --       (lam d. match d with DeclInclude r then Some r.path else None ())
    --       decls
    --   in

    --   let p = parseProgram content in
    --   use MLangPrettyPrint in
    --   eprintln (mlang2str p);
    --   eprintln (join ["includes: ", strJoin ", " (getIncludeStrings p)]);
    --   ()
    -- );

    -- let paths = strSplit "/" (stripUriProtocol uri) in
    -- let directory = strJoin "/" (init paths) in
    -- let file = last paths in
    -- let tmpFilePath = join [directory, "/", file, temp_file_extension] in

    -- eprintln (join ["Reindexing MCore file '", uri, "'"]);
    -- writeFile tmpFilePath content;
    -- let result = compileFunc parameters tmpFilePath in

    -- sysDeleteFile tmpFilePath;
    -- result

mexpr

let environment: LSPEnvironment = {
  files = mapEmpty cmpString
} in

eprintln "Miking MCore LSP started";
readJsonRPC compileFunc environment;
eprintln "Miking MCore LSP ended"

