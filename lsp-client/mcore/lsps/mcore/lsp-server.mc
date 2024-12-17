include "mlang/main.mc"
include "../../lsp-server/lsp-server.mc"
include "./parse-error.mc"

type MLangFile = {
  content: String,
  symEnv: Option SymEnv
}

type MLangEnvironment = {
  files: Map String MLangFile,
  dependencies: Map String (Set String)
}

type Diagnostic = (Info, String)

type FileLoader = {
  load: Info -> String -> Result Diagnostic Diagnostic MLangFile,

  -- Mark a file as a dependency of another file
  addDependency: String -> String -> ()
}

-- Set the filename of the info for an error
let errorWithFilename: String -> Diagnostic -> Diagnostic =
  lam filename. lam err.
    match err with (info, msg) in
      (infoWithFilename filename info, msg)

type PartialResult w e a = {
  warnings: Map Symbol w,
  errors: Map Symbol e,
  value: a
}

-- type MLangProgramResult = use MLangAst in {
--   errors: [Diagnostic],
--   warnings: [Diagnostic],
--   program: MLangProgram
-- }

type MLangProgramResult = use MLangAst in Result Diagnostic Diagnostic MLangProgram

let flattenErrors: all w. all e. all a. [Result w e a] -> Result w e [a] =
  use MLangAst in
  lam results.
    foldl (lam acc. lam val. result.map2 (lam a1. lam a2. join [a1, [a2]]) acc val) (result.ok []) results

recursive let populateMLangExprInfoWithFilename: use MLangAst in String -> Expr -> Expr =
  use MLangAst in
  use MExprAst in
  lam filename. lam expr.
    let expr = withInfo (infoWithFilename filename (infoTm expr)) expr in
    smap_Expr_Expr (populateMLangExprInfoWithFilename filename) expr
end

recursive let populateMLangDeclInfoWithFilename: use MLangAst in String -> Decl -> Decl =
  use MLangAst in
  lam filename. lam decl.
    let decl = declWithInfo (infoWithFilename filename (infoDecl decl)) decl in 
    let decl = smap_Decl_Decl (populateMLangDeclInfoWithFilename filename) decl in
    smap_Decl_Expr (populateMLangExprInfoWithFilename filename) decl
end

let populateMLangProgramInfoWithFilename: use MLangAst in String -> MLangProgram -> MLangProgram =
  lam filename. lam program.
    let decls = map (populateMLangDeclInfoWithFilename filename) program.decls in
    let expr = populateMLangExprInfoWithFilename filename program.expr in
    {
      program with
      decls = decls,
      expr = expr
    }

lang MLangModularIncludeHandler = MLangAst + BootParserMLang
  sem parseAndHandleIncludes : FileLoader -> String -> MLangProgramResult
  sem parseAndHandleIncludes loader =| path -> 
    let dir = eraseFile path in 
    let libs = addCWDtoLibs (parseMCoreLibsEnv ()) in
    let included = ref (setEmpty cmpString) in 
    match result.consume (loader.load testinfo_ path) with (_, Right file) in
    handleIncludesFile loader included dir libs path file

  sem handleIncludesProgram : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangProgram -> MLangProgramResult 
  sem handleIncludesProgram loader included dir libs path =| prog ->
    let decls = map (flattenIncludes loader included dir libs path) prog.decls in
    let decls = flattenErrors decls in
    let consumeDecls = lam decls.
      let f = lam decls. lam decl. concat decls decl in
        let decls: [Decl] = foldl f [] decls in
        {prog with decls = decls}
    in
    result.map consumeDecls decls

  sem handleIncludesFile : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgramResult
  sem handleIncludesFile loader included dir libs path =| file ->
    let s = deref included in 

    if setMem path s then 
      result.ok {decls = [], expr = uunit_}
    else 
      let handleProgram = lam prog.
        modref included (setInsert path s);
        handleIncludesProgram loader included dir libs path prog
      in

      let parseMLangString = lam file. lam path.
        let res = result.map (populateMLangProgramInfoWithFilename path) (parseMLangString file.content) in
        mapErrors (errorWithFilename path) res
      in

      result.bind (parseMLangString file path) handleProgram

  sem flattenIncludes : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> Decl -> Result Diagnostic Diagnostic [Decl]
  sem flattenIncludes loader included dir libs parentPath =
  | DeclInclude {path = path, info = info} ->

    let consumePath = lam path.
      let consumeContent = lam file.
        let consumeProgram = lam prog. prog.decls in
        result.map consumeProgram (handleIncludesFile loader included (eraseFile path) libs path file)
      in
      let res = result.bind (loader.load info path) consumeContent in
      loader.addDependency path parentPath;
      res
    in

    let completePath = findPath loader dir libs info path in
    let decls = result.bind completePath consumePath in

    match result.consume decls with (warnings, declsResult) in
    switch declsResult
      case Right decls then
        result.ok decls
      case Left _ then
        let additionalError = result.err (info, join [
          "File '",
          path,
          "' could not be parsed!"
        ]) in
        result.map2 (lam a1. lam a2. join [a1, a2]) decls additionalError
    end
  | other -> result.ok [other]

  sem findPath : FileLoader -> String -> Map String String -> Info -> String -> Result Diagnostic Diagnostic String
  sem findPath loader dir libs info =| path ->
    let libs = mapInsert "current" dir libs in 
    let prefixes = mapValues libs in 
    let paths = map (lam prefix. filepathConcat prefix path) prefixes in 

    let existingFiles = filter sysFileExists paths in 
    let existingFilesAsSet = setOfSeq cmpString existingFiles in 

    switch (setSize existingFilesAsSet)
      case 0 then 
        result.err (info, "File not found!")
      case 1 then 
        result.ok (head (setToSeq existingFilesAsSet))
      case _ then 
        -- TODO(voorberg, 09/05/2024): This happens because we dont properly
        -- deal with libraries yet. The code does not yet realise that 
        -- some absolute path is equal to some relative path.
        warnSingle [info] "Multiple files found" ;
        result.ok (head (setToSeq existingFilesAsSet))
    end
end

-- let addDeclToSymEnv = lam compileMLang. lam uri. lam symEnv. lam decl.
--   use MLangAst in
--   switch decl
--     case DeclInclude { path=path, info=info } then
--       let path = filepathConcat (eraseFile uri) path in
--       -- let libs = addCWDtoLibs (parseMCoreLibsEnv ()) in
--       -- let included = ref (setEmpty cmpString) in
--       eprintln (join ["Including MCore file '", path, "'"]);
--       let exists = fileExists path in
--       eprintln (join ["File '", path, "' exists: ", bool2string exists]);
--       if not exists then result.ok symEnv else
--       match optionMap fileReadString (fileReadOpen path) with Some content in
--       match result.consume (compileMLang path content) with (warnings, compileResult) in
--       switch compileResult
--         case Right (program, newSymEnv) then
--           result.ok (mergeNameEnv symEnv newSymEnv)
--         -- case compileResult with Left (_, errors) then
--         --   let errors = map (errorWithFilename path) errors in
--         --   foldl1 result.withAnnotations errors
--       end
--     case _ then
--       result.ok symEnv
--   end

recursive let compileMLang =
  lam loader. lam uri.
    use MLangPipeline in

    let uri = stripUriProtocol uri in
    let parseResult = (use MLangModularIncludeHandler in parseAndHandleIncludes loader uri) in

    let handleProgram = lam program.
      let program = constTransformProgram builtin program in
      let program = composeProgram program in
      match symbolizeMLang symEnvDefault program with (symEnv, program) in
      match result.consume (checkComposition program) with (_, res) in

      switch res 
        case Left errs then 
          -- Todo: remove and report errors
          iter raiseError errs ;
          never
        case Right env then
          let ctx = _emptyCompilationContext env in
          let res = result.consume (compile ctx program) in
          match res with (_, rhs) in
          match rhs with Right expr in
          let expr = postprocess env.semSymMap expr in
          result.ok (program, symEnv)
      end
    in

    result.bind parseResult handleProgram

    -- match result.consume (parseMLangString content) with (warnings, parseResult) in
    -- match result.consume parseResult with (warnings, parseResult) in
    
    -- match parseResult with e & Left errors then
    --   eprintln (join ["Failed to parse MCore file '", uri, "'"]);
    --   let createFileError = compose result.err (errorWithFilename uri) in
    --   let errors = map createFileError errors in
    --   foldl result.withAnnotations [] errors

    -- else match parseResult with Right program in
    --   eprintln (join ["Parsed MCore file '", uri, "'"]);
    --   let program = constTransformProgram builtin program in
    --   let program = composeProgram program in

    --   -- let symEnv = foldl (addDeclToSymEnv compileMLang uri) symEnvDefault program.decls in
    --   -- match result.consume () -- todo: extract result from symenv

    --   -- match symbolizeMLang symEnvDefault program with (_, program) in

    --   use MLangPrettyPrint in eprintln (mlang2str program);
    --   -- result.ok (program, symEnv)
    --   result.ok (program, symEnvDefault)
end

let compileFunc: Ref MLangEnvironment -> CompilationParameters -> Map String CompilationResult =
  lam mLangEnvironment. lam parameters.
    let uri = parameters.uri in
    let content = parameters.content in
    let strippedUri = stripUriProtocol uri in

    let environment = deref mLangEnvironment in
    modref mLangEnvironment {
      environment with
      files = mapInsert strippedUri {
        content = content,
        symEnv = None ()
      } environment.files
    };

    let load = lam info. lam path.
      let environment = deref mLangEnvironment in
      match mapLookup path environment.files with Some file then
        result.ok file
      else
        let exists = fileExists path in
        if not exists then result.err (info, "File not found!") else
        match optionMap fileReadString (fileReadOpen path) with Some content in
        let file: MLangFile = {
          content = content,
          symEnv = None ()
        } in
        modref mLangEnvironment {
          environment with
          files = mapInsert path file environment.files
        };
        result.ok file
    in

    let addDependency = lam parent. lam child.
      let environment = deref mLangEnvironment in
      let prev = match mapLookup parent environment.dependencies with Some deps then deps else setEmpty cmpString in
      let newDeps = setInsert child prev in
      modref mLangEnvironment {
        environment with
        dependencies = mapInsert parent newDeps environment.dependencies
      }
    in

    let getChildren = lam.
      let environment = deref mLangEnvironment in
      match mapLookup strippedUri environment.dependencies with Some deps then deps else setEmpty cmpString
    in

    let previousChildren = getChildren () in

    let loader: FileLoader = {
      load = load,
      addDependency = addDependency
    } in

    let compileFile = lam uri.
      match result.consume (compileMLang loader uri) with (warnings, compilationResult) in
      match compilationResult with Left errors then
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
              info = makeInfo {filename=parameters.uri, row=1, col=1} {filename=parameters.uri, row=1, col=100},
              pprint = lam. "Hejsan svejsan",
              lookupDefinition = None ()
            }),
            lenses = []
          })
      ]
    in

    let newChildren = getChildren () in
    let children = setToSeq (setUnion previousChildren newChildren) in

    let paths = join [[strippedUri], children] in

    eprintln (join ["Files: ", join (map (lam path. join ["'", path, "', "]) paths)]);

    let res = foldl (lam acc. lam path. mapUnion acc (compileFile path)) (mapEmpty cmpString) paths in

    eprintln (join ["Result: ", join (mapKeys res)]);

    res

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
  files = mapEmpty cmpString,
  dependencies = mapEmpty cmpString
} in

let mLangEnvironment = ref mLangEnvironment in

let lspStartParameters: LSPStartParameters = {
  onOpen   = compileFunc mLangEnvironment,
  onChange = compileFunc mLangEnvironment,
  onClose  = onClose mLangEnvironment,
  options  = {
    defaultLSPOptions with
    pruneMessages = false
  }
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"