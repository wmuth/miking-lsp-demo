include "mlang/main.mc"
include "../../lsp-server/lsp-server.mc"

lang MLangAndMExpr = MLangAst + MExprAst
end

type Diagnostic = (Info, String)

type MLangProgramResult = use MLangAst in Result Diagnostic Diagnostic MLangProgram

type MLangFile = {
  content: String,
  parsed: Option MLangProgramResult,
  includeCache: Map String (use MLangAst in Result Diagnostic Diagnostic [Decl]),
  symEnv: Option SymEnv
}

let emptyFile = {
  content = "",
  parsed = None (),
  includeCache = mapEmpty cmpString,
  symEnv = None ()
}

type MLangEnvironment = {
  files: Map String MLangFile,
  dependencies: Map String (Set String)
}

type FileLoader = {
  load: Info -> String -> Result Diagnostic Diagnostic MLangFile,
  
  -- Store a parsed file
  store: String -> MLangFile -> (),

  -- Mark a file as a dependency of another file
  addDependency: String -> String -> ()
}

let ssMapToString = lam m.
  let f = lam acc. lam key. lam val. join [acc, key, " -> ", val, "\n"] in
  mapFoldWithKey f "" m

-- Set the filename of the info for an error
let errorWithFilename: String -> Diagnostic -> Diagnostic =
  lam filename. lam err.
    match err with (info, msg) in
      (infoWithFilename filename info, msg)

-- type PartialResult w e a = {
--   warnings: Map Symbol w,
--   errors: Map Symbol e,
--   value: a
-- }

-- type MLangProgramResult = use MLangAst in {
--   errors: [Diagnostic],
--   warnings: [Diagnostic],
--   program: MLangProgram
-- }

let flattenErrors: all w. all e. all a. [Result w e a] -> Result w e [a] =
  use MLangAst in
  lam results.
    foldl (lam acc. lam val. result.map2 (lam a1. lam a2. join [a1, [a2]]) acc val) (result.ok []) results

recursive let populateMLangExprInfoWithFilename: use MLangAndMExpr in String -> Expr -> Expr =
  use MLangAndMExpr in
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

let createMultipleFilesFoundWarning = lam existingFilesAsSet.
  let sep = "\n * " in
  join [
    "Multiple files found: ",
    sep,
    strJoin sep (setToSeq existingFilesAsSet),
    "\nUsing: '",
    head (setToSeq existingFilesAsSet),
    "'"
  ]

lang MLangModularIncludeHandler = MLangAst + BootParserMLang
  sem parseAndHandleIncludes : FileLoader -> String -> MLangProgramResult
  sem parseAndHandleIncludes loader =| path -> 
    let dir = eraseFile path in 
    let libs = addCWDtoLibs (parseMCoreLibsEnv ()) in
    let included = ref (setEmpty cmpString) in

    match result.consume (loader.load testinfo_ path) with (_, Right file) in
    handleIncludesFile loader included dir libs path file

  sem handleIncludesProgram : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgram -> MLangProgramResult 
  sem handleIncludesProgram loader included dir libs path file =| prog ->
    let f = lam decl.
      let result = flattenIncludes loader included dir libs file path decl in
      -- loader.store path {
      --   file with
      --   includeCache = mapInsert (normalizeFilePath path) result file.includeCache
      -- };
      result
    in
    let decls = map f prog.decls in
    let decls = flattenErrors decls in
    let consumeDecls = lam decls.
      let f = lam decls. lam decl. concat decls decl in
      {prog with decls = foldl f [] decls}
    in
    result.map consumeDecls decls

  sem handleIncludesFile : FileLoader -> Ref (Set String) -> String -> Map String String -> String -> MLangFile -> MLangProgramResult
  sem handleIncludesFile loader included dir libs path =| file ->
    let path = normalizeFilePath path in
    let s = deref included in 

    if setMem path s then 
      result.ok {decls = [], expr = uunit_}
    else 
      let handleProgram = lam prog.
        modref included (setInsert path s);
        handleIncludesProgram loader included dir libs path file prog
      in

      let parseMLangString = lam file. lam path.
        let orElse = lam.
          let res = result.map (populateMLangProgramInfoWithFilename path) (parseMLangString file.content) in
          let res = mapErrors (errorWithFilename path) res in
          loader.store path {
            file with
            parsed = Some res
          };
          res
        in

        (
          match file.parsed
            with Some parsed then eprintln (join ["Already parsed: ", path])
            else eprintln (join ["Parsing: ", path])
        );
        
        optionGetOrElse orElse file.parsed
      in

      result.bind (parseMLangString file path) handleProgram

  sem flattenIncludes : FileLoader -> Ref (Set String) -> String -> Map String String -> MLangFile -> String -> Decl -> Result Diagnostic Diagnostic [Decl]
  sem flattenIncludes loader included dir libs file childPath =
  | DeclInclude {path = path, info = info} ->
    let consumePath = lam path.
      let path = normalizeFilePath path in
      loader.addDependency path childPath;

      eprintln (join ["[", childPath ,"] Including: ", path]);
      match mapLookup path file.includeCache with Some included then
        eprintln (join ["[", childPath ,"] Cache hit: ", path]);
        included
      else
        let consumeContent = lam file.
          let program = handleIncludesFile loader included (eraseFile path) libs path file in
          result.map (lam prog. prog.decls) program
        in
        let res = result.bind (loader.load info path) consumeContent in
        eprintln (join ["[", childPath ,"] Adding cache: ", path]);
        loader.store (normalizeFilePath childPath) {
          file with
          includeCache = mapInsert path res file.includeCache
        };
        res
    in

    let completePath = findPath loader dir libs info path in
    let decls = result.bind completePath consumePath in

    match result.consume decls with (_, declsResult) in
    switch declsResult
      case Right declsOk then
        result.withAnnotations decls (result.ok declsOk)
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
        let warning = result.warn (info, createMultipleFilesFoundWarning existingFilesAsSet) in
        result.withAnnotations warning (result.ok (head (setToSeq existingFilesAsSet)))
    end
end



recursive let compileMLang: FileLoader -> String -> use MLangAst in Result Diagnostic Diagnostic (MLangProgram, SymEnv) =
  lam loader. lam uri.
    use MLangPipeline in

    let uri = stripUriProtocol uri in
    let parseResult = (use MLangModularIncludeHandler in parseAndHandleIncludes loader uri) in

    let handleProgram = lam program.
      -- eprintln "Const transforming program";
      -- let program = constTransformProgram builtin program in
      -- eprintln "Done const transforming program";
      -- eprintln "Composing program";
      -- let program = composeProgram program in
      -- eprintln "Done composing program";

      -- -- Todo: symbolizeMLang is currently crashing while encountering undefined language fragments
      -- -- match symbolizeMLang symEnvDefault program with (symEnv, program) in
      -- eprintln "Checking composition";
      -- match result.consume (checkComposition program) with (_, res) in
      -- eprintln "Done checking composition";

      -- switch res 
      --   case Left errs then 
      --     -- Todo: remove and report errors
      --     eprintln "TODO: We reached a never in `compileMLang`";
      --     iter raiseError errs ;
      --     never
      --   case Right env then
      --     -- let ctx = _emptyCompilationContext env in
      --     -- let res = result.consume (compile ctx program) in
      --     -- match res with (_, rhs) in
      --     -- match rhs with Right expr in
      --     -- let expr = postprocess env.semSymMap expr in
      --     -- result.ok (program, symEnv)
      --     result.ok (program, symEnvDefault)
      -- end

      result.ok (program, symEnvDefault)
    in

    result.bind parseResult handleProgram
end

recursive let getDirtyDependencies: Map String (Set String) -> Set String -> String -> Set String =
  lam dependencies. lam dirty. lam path.
    if setMem path dirty then
      dirty
    else
      let dirty = setInsert path dirty in
      match mapLookup path dependencies with Some children then
        foldl (getDirtyDependencies dependencies) dirty (setToSeq children)
      else
        dirty
end


let compileFunc: Ref MLangEnvironment -> CompilationParameters -> Map String CompilationResult =
  lam mLangEnvironment. lam parameters.
    let uri = parameters.uri in
    let content = parameters.content in
    let strippedUri = stripUriProtocol uri in
    let environment = deref mLangEnvironment in

    let dirtyPaths = getDirtyDependencies environment.dependencies (setEmpty cmpString) strippedUri in
    eprintln (join ["Dirty paths: ", strJoin ", " (setToSeq dirtyPaths)]);

    let pathClean = lam key. lam. not (setMem key dirtyPaths) in
    modref mLangEnvironment {
      environment with
      files = mapMap (
        lam file.
          {
            file with
            includeCache = mapFilterWithKey pathClean file.includeCache
          }  
      ) environment.files
    };

    let store: String -> MLangFile -> () = lam path. lam file.
      let path = normalizeFilePath path in
      let environment = deref mLangEnvironment in
      modref mLangEnvironment {
        environment with
        files = mapInsert path file environment.files
      };
      ()
    in

    let load = lam info. lam path.
      let path = normalizeFilePath path in
      let environment = deref mLangEnvironment in
      match mapLookup path environment.files with Some file then
        result.ok file
      else
        let exists = fileExists path in
        if not exists then result.err (info, "File not found!") else
        match optionMap fileReadString (fileReadOpen path) with Some content in
        let file: MLangFile = {
          content = content,
          includeCache = mapEmpty cmpString,
          parsed = None (),
          symEnv = None ()
        } in
        store path file;
        result.ok file
    in

    let addDependency = lam parent. lam child.
      let parent = normalizeFilePath parent in
      let child = normalizeFilePath child in
      let environment = deref mLangEnvironment in
      let prev = match mapLookup parent environment.dependencies with Some deps then deps else setEmpty cmpString in
      let newDeps = setInsert child prev in

      modref mLangEnvironment {
        environment with
        dependencies = mapInsert parent newDeps environment.dependencies
      }
    in

    -- let getChildren = lam.
    --   let environment = deref mLangEnvironment in
    --   match mapLookup strippedUri environment.dependencies with Some deps then deps else setEmpty cmpString
    -- in
    -- let previousChildren = getChildren () in
    -- let children = getChildren () in

    let loader: FileLoader = {
      load = load,
      store = store,
      addDependency = addDependency
    } in

    let currentFile = optionGetOr emptyFile (mapLookup strippedUri environment.files) in

    -- Reset/create the file in the environment
    loader.store strippedUri {
      currentFile with
      content = content,
      parsed = None (),
      symEnv = None ()
    };

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

    -- let newChildren = getChildren () in
    -- let children = setToSeq (setUnion previousChildren newChildren) in

    -- let paths = join [[strippedUri], children] in
    let paths = setToSeq (setOfSeq cmpString (join [[strippedUri], setToSeq dirtyPaths])) in

    eprintln (join ["Compiling files: ", join (map (lam path. join ["'", path, "', "]) paths)]);
    
    foldl (lam acc. lam path. mapUnion acc (compileFile path)) (mapEmpty cmpString) paths

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