include "mlang/main.mc"
include "../../lib/utils.mc"
include "./file.mc"
include "./file-loader.mc"
include "./include-handler.mc"
include "./parser.mc"
include "./symbolize.mc"
include "./utests.mc"
include "./lookup.mc"

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

type FileLoader = {
  load: Path -> MLangFile,
  store: Path -> MLangFile -> ()
}

type MLangProgramResult = use MLangAst in Result Diagnostic Diagnostic MLangProgram

lang MLangCompiler = MLangAst + MExprAst + MLangParser + MLangIncludeHandler + MLangFileLoader + MLangFileHandler + MLangSymbolize
  sem upgradeFileKind: CompilationParameters -> FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFileKind -> MLangFile
  sem upgradeFileKind parameters loader getFile path file =
  | Loaded { content = content } ->
    let file = parseMLang path content in
    upgradeFile parameters loader getFile path file
  | Parsed { program = program } ->
    let f = lam pathInfo.
      match pathInfo with (info, path, file) in
      if leqi (length file.errors) 0 then
        None ()
      else
        Some (info, join ["File '", path, "' contains errors!"])
    in
    
    let includes = map (lam v. (v.0, v.1, getFile v.1)) (getIncludePaths file.kind) in
    let includeErrors = filterOption (map f includes) in

    let file = {
      file with
      errors = join [file.errors, includeErrors]
    } in

    let includedFiles = map (lam x. x.2) includes in
    let file = symbolizeMLang path includedFiles file in
    
    upgradeFile parameters loader getFile path file
  | _ -> file

  sem upgradeFile : CompilationParameters -> FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFile
  sem upgradeFile parameters loader getFile path =
  | file ->
    let file = upgradeFileKind parameters loader getFile path file file.kind in
    loader.store path file;
    parameters.notify path {
      errors = file.errors,
      warnings = file.warnings
    };
    file

  sem compile: LSPConfig -> Ref MLangEnvironment -> CompilationParameters -> Map Path CompilationResult
  sem compile config mLangEnvironment =| parameters ->
    let uri = stripUriProtocol parameters.uri in
    let content = parameters.content in
    let environment = deref mLangEnvironment in

    -- let dirtyPaths = getDirtyDependencies environment.dependencies (setEmpty cmpString) uri in
    -- eprintln (join ["Dirty paths: ", strJoin ", " (setToSeq dirtyPaths)]);

    let loader = createLoader mLangEnvironment in

    -- Reset/create the file in the environment
    -- TODO: Downgrade dependent files to Parsed
    loader.store uri {
      kind = Loaded { content = content },
      errors = [],
      warnings = []
    };

    let getCompilationResult: MLangFile -> CompilationResult =
      lam file.
        let lenses = use MLangUtestLenses in getUtestLenses file in
        let lookup = createLookup [
          use MLangLookupIncludeLookup in includesLookup file
        ] in

        {
          errors = file.errors,
          warnings = file.warnings,
          lookup = lookup uri,
          lenses = lenses
        }
    in

    recursive let getFile: Path -> MLangFile = 
      lam uri.
        eprintln (join ["Compiling file: ", uri]);

        let file = loader.load uri in
        upgradeFile parameters loader getFile uri file
    in

    let file = getFile uri in
    let compilationResult = getCompilationResult file in

    mapFromSeq cmpString [(
      uri,
      compilationResult
    )]
    
    -- let paths = setToSeq (setOfSeq cmpString (join [[strippedUri], setToSeq dirtyPaths])) in
    -- eprintln (join ["Compiling files: ", join (map (lam path. join ["'", path, "', "]) paths)]);
end

mexpr

let emptyInfo = makeInfo {filename = "", row = 0, col = 0} {filename = "", row = 0, col = 0} in

use MLangCompilationKind in

utest getIncludes (
  Loaded { content = "content" }
) with [] in

utest getIncludes (
  Parsed { content = "content", parsed = { decls = [], expr = uunit_ }, includes = [
    (emptyInfo, ExistingFile "path/./"), (emptyInfo, NonExistentFiles ["abc"])
  ] }
) with ["path"] in

()