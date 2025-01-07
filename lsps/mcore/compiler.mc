include "mlang/main.mc"
include "../../lib/utils.mc"
include "./file.mc"
include "./file-loader.mc"
include "./include-handler.mc"
include "./parser.mc"
include "./symbolize.mc"
include "./utests.mc"
include "./lookup.mc"

type MLangFile =  use MLangFileHandler in MLangFile

type MLangEnvironment = {
  files: Map String MLangFile,
  dependencies: Map String (Set String)
}

-- recursive let compileMLang: FileLoader -> String -> use MLangAst in Result Diagnostic Diagnostic (MLangProgram, SymEnv) =
--   lam loader. lam uri.
--     use MLangPipeline in

--     let uri = stripUriProtocol uri in
    -- let parseResult = (use MLangModularIncludeHandler in parseAndHandleIncludes loader uri) in

    -- let handleProgram = lam program.
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

--       result.ok (program, symEnvDefault)
--     in

--     result.bind parseResult handleProgram
-- end

-- let compileFunc: LSPConfig -> Ref MLangEnvironment -> CompilationParameters -> Map Path CompilationResult =
--   lam config. lam mLangEnvironment. lam parameters.

recursive let getDirtyDependencies: Map Path (Set Path) -> Set Path -> Path -> Set Path =
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

let createLookup: [[(Info, LookupResult)]] -> Path -> Int -> Int -> Option LookupResult =
  lam lookups. lam uri. lam row. lam col.
    recursive let findElement: [(Info, LookupResult)] -> Option LookupResult =
      lam xs.
        match xs with [x] ++ xs then
          match x with (info, lookupResult) in
          if infoCollision info uri row col then
            Some lookupResult
          else
            findElement xs
        else
          None ()
    in

    findElement (foldl (lam acc. lam lookup. join [acc, lookup]) [] lookups)

type FileLoader =  {
  load: Path -> MLangFile,
  store: Path -> MLangFile -> ()
}

type MLangProgramResult = use MLangAst in Result Diagnostic Diagnostic MLangProgram

lang MLangCompiler =
  MLangAst + MExprAst + MLangParser +
  MLangIncludeHandler + MLangFileLoader +
  MLangFileHandler + MLangSymbolize

  sem upgradeFileInner: CompilationParameters -> FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFile
  sem upgradeFileInner parameters loader getFile path =
  | file -> file
  | CLoaded { content = content } ->
    let file = parseMLang path content in
    upgradeFile parameters loader getFile path file
  | file & CParsed parsed ->
    let f = lam pathInfo.
      match pathInfo with (info, path, file) in
      if leqi (length (getFileErrors file)) 0 then
        None ()
      else
        Some (info, join ["File '", path, "' contains errors!"])
    in
    
    let links = map (lam v. (v.0, v.1, getFile v.1)) (getIncludePaths file) in
    let linkErrors = filterOption (map f links) in

    let file = CLinked {
      parsed = parsed,
      links = links,
      linkErrors = linkErrors,
      warnings = []
    } in

    upgradeFile parameters loader getFile path file
  | file & CLinked { links = links } ->
    let linkedFiles = map (lam x. x.2) links in
    let file = symbolizeMLang path linkedFiles file in
    
    upgradeFile parameters loader getFile path file

  sem upgradeFile : CompilationParameters -> FileLoader -> (Path ->  MLangFile) -> Path ->  MLangFile ->  MLangFile
  sem upgradeFile parameters loader getFile path =
  | file ->
    let file = upgradeFileInner parameters loader getFile path file in
    loader.store path file;
    parameters.notify path {
      errors = getFileErrors file,
      warnings = getFileWarnings file
    };
    file

  sem downgradeSymbolizedFile : MLangFile -> MLangFile
  sem downgradeSymbolizedFile =
  | file -> file
  | CLinked { parsed = parsed } -> CParsed parsed
  | CSymbolized { linked = linked } -> downgradeSymbolizedFile (CLinked linked)

  sem compile: LSPConfig -> Ref MLangEnvironment -> CompilationParameters -> Map Path CompilationResult
  sem compile config mLangEnvironment =| parameters ->
    let uri = stripUriProtocol parameters.uri in
    let content = parameters.content in
    let environment = deref mLangEnvironment in

    -- Convert Map DependentPath (Set DependencyPaths) to Map DependencyPath (Set DependentPaths)
    let dependencyGraph: Map Path (Set Path) = foldl (
      lam acc. lam valueKey.
        match valueKey with (key, values) in
        let values = setToSeq values in
        foldl (
          lam acc. lam value.
            mapInsertWith setUnion value (setOfSeq cmpString [key]) acc
        ) acc values
    ) (mapEmpty cmpString) (mapToSeq environment.dependencies) in

    let dirtiedFiles = getDirtyDependencies dependencyGraph (setEmpty cmpString) uri in
    eprintln (join ["Dirty paths: ", strJoin ", " (setToSeq dirtiedFiles)]);

    let dirtiedFiles = setFilter (lam furi. not (eqString furi uri)) dirtiedFiles in
    let dirtiedFiles = setToSeq dirtiedFiles in
    let dirtiedFiles = join [[uri], dirtiedFiles] in

    let loader = createLoader mLangEnvironment in

    iter (
      lam uri.
        loader.store uri (downgradeSymbolizedFile (loader.load uri))
    ) dirtiedFiles;

    -- Reset/create the file in the environment
    loader.store uri (CLoaded { content = content });

    recursive let getFile: Path -> MLangFile = 
      lam uri.
        eprintln (join ["Compiling file: ", uri]);
        let file = loader.load uri in
        upgradeFile parameters loader getFile uri file
    in

    let getCompilationResult: Path -> CompilationResult =
      lam uri.
        let file = getFile uri in
        let lenses = use MLangUtestLenses in getUtestLenses file in
        let lookup = createLookup [
          use MLangLookupIncludeLookup in includesLookup file
        ] in

        {
          emptyCompilationResult with
          errors = getFileErrors file,
          warnings = getFileWarnings file,
          lookup = lookup uri,
          -- lenses = lenses
          lenses = []
        }
    in

    let getCompilationResults: [Path] -> Map Path CompilationResult =
      lam uris.
        let results = map (
          lam uri.
            let compilationResult = getCompilationResult uri in
            (uri, compilationResult)
        ) uris in

        mapFromSeq cmpString results
    in

    -- Todo: We could possibly check dependent files in the
    -- compile function, after having parsed, reducing traversing
    -- the entire dependency tree every time.
    let seenDependentFiles = ref (setEmpty cmpString) in
    recursive let registerDependencies = lam uri.
      -- Early return if we've already seen this file
      if setMem uri (deref seenDependentFiles) then () else
      modref seenDependentFiles (setInsert uri (deref seenDependentFiles));
      let file = loader.load uri in
      let dependencies = getIncludePathStrings file in
      let environment = deref mLangEnvironment in
      modref mLangEnvironment {
        environment with
        dependencies = mapInsert uri (setOfSeq cmpString dependencies) environment.dependencies
      };
      iter registerDependencies dependencies
    in

    let results = getCompilationResults dirtiedFiles in
    registerDependencies uri;

    iter (
      lam v.
        match v with (uri, file) in
          eprintln (join ["File: ", uri, ", status: ", printFileKind file]);
          ()
    ) (mapToSeq (deref mLangEnvironment).files);

    results
end