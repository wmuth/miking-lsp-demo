include "mlang/main.mc"
include "../../lsp-server/lsp-server.mc"
include "./parse-error.mc"

type MLangEnvironment = {
  files: Map String SymEnv
}

-- Set the filename of the info for an error
let errorWithFilename: String -> (Info, String) -> (Info, String) =
  lam filename. lam err.
    match err with (info, msg) in
      (infoWithFilename filename info, msg)

let addDeclToSymEnv = lam compileMLang. lam uri. lam symEnv. lam decl.
  use MLangAst in
  switch decl
    case DeclInclude { path=path, info=info } then
      let path = filepathConcat (eraseFile uri) path in
      -- let libs = addCWDtoLibs (parseMCoreLibsEnv ()) in
      -- let included = ref (setEmpty cmpString) in
      eprintln (join ["Including MCore file '", path, "'"]);
      let exists = fileExists path in
      eprintln (join ["File '", path, "' exists: ", bool2string exists]);
      if not exists then result.ok symEnv else
      match optionMap fileReadString (fileReadOpen path) with Some content in
      match result.consume (compileMLang path content) with (warnings, compileResult) in
      switch compileResult
        case Right (program, newSymEnv) then
          result.ok (mergeNameEnv symEnv newSymEnv)
        -- case compileResult with Left (_, errors) then
        --   let errors = map (errorWithFilename path) errors in
        --   foldl1 result.withAnnotations errors
      end
    case _ then
      result.ok symEnv
  end

recursive
  let compileMLang =
    lam uri. lam content.
      use MLangPipeline in

      let uri = stripUriProtocol uri in

      match result.consume (parseMLangString content) with (warnings, parseResult) in
      match parseResult with e & Left errors then
        eprintln (join ["Failed to parse MCore file '", uri, "'"]);
        let createFileError = compose result.err (errorWithFilename uri) in
        let errors = map createFileError errors in
        foldl1 result.withAnnotations errors
      else match parseResult with Right program in
      eprintln (join ["Parsed MCore file '", uri, "'"]);
      let program = constTransformProgram builtin program in
      let program = composeProgram program in

      -- let symEnv = foldl (addDeclToSymEnv compileMLang uri) symEnvDefault program.decls in
      -- match result.consume () -- todo: extract result from symenv

      -- match symbolizeMLang symEnvDefault program with (_, program) in

      use MLangPrettyPrint in eprintln (mlang2str program);
      -- result.ok (program, symEnv)
      result.ok (program, symEnvDefault)
end

let compileFunc: Ref MLangEnvironment -> CompilationParameters -> Map String CompilationResult =
  lam mLangEnvironment. lam parameters.
    let uri = parameters.uri in
    let content = parameters.content in

    match result.consume (compileMLang uri content) with (warnings, compilationResult) in
    match compilationResult with Left errors then
      let errors = map (errorWithFilename uri) errors in
      mapFromSeq cmpString [
        (uri, {
          errors = errors,
          warnings = warnings,
          lookup = lam. lam. None (),
          lenses = []
        })
      ]
    else match compilationResult with Right (program, symEnv) in
      mapFromSeq cmpString [
        (uri, {
          errors = [],
          warnings = warnings,
          lookup = lam. lam. Some ({
            info = makeInfo {filename = parameters.uri, row=1, col = 1} {filename = parameters.uri, row=1, col = 100},
            pprint = lam. "Hejsan svejsan",
            lookupDefinition = None ()
          }),
          lenses = []
        })
      ]

    -- match symbolizeMLang symEnvDefault program with (_, program) in
    -- match result.consume (checkComposition p) with (_, res) in
    -- (switch res
    --   case Right env then
    --     let ctx = _emptyCompilationContext env in 
    --     let res = result.consume (compile ctx p) in 
    --     match res with (_, rhs) in 
    --     match rhs with Right expr in
    --     printLn (expr2str expr);
    --     ()
    -- end);

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

    -- {
    --   expr = None (),
    --   errors = [],
    --   warnings = warnings
    -- }

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

let onClose: Ref MLangEnvironment -> String -> () =
  lam mLangEnvironment. lam uri.
    let dereffed = deref mLangEnvironment in
    let newMLangEnvironment = {
      dereffed with
      files = mapRemove uri dereffed.files
    } in
    modref mLangEnvironment newMLangEnvironment

mexpr

let mLangEnvironment = {
  files = mapEmpty cmpString
} in

let lspStartParameters: LSPStartParameters = {
  onOpen   = compileFunc (ref mLangEnvironment),
  onChange = compileFunc (ref mLangEnvironment),
  onClose  = onClose (ref mLangEnvironment),
  options  = {
    defaultLSPOptions with
    pruneMessages = false
  }
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"