include "mlang/main.mc"
include "../../lsp-server/lsp-server.mc"
include "./include-handler.mc"

type MLangEnvironment = {
  files: Map String MLangFile,
  dependencies: Map String (Set String)
}

let ssMapToString = lam m.
  let f = lam acc. lam key. lam val. join [acc, key, " -> ", val, "\n"] in
  mapFoldWithKey f "" m

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

-- recursive let compileMLang: FileLoader -> String -> use MLangAst in Result Diagnostic Diagnostic (MLangProgram, SymEnv) =
--   lam loader. lam uri.
--     use MLangPipeline in

--     let uri = stripUriProtocol uri in
--     -- let parseResult = (use MLangModularIncludeHandler in parseAndHandleIncludes loader uri) in

--     let handleProgram = lam program.
--       -- eprintln "Const transforming program";
--       -- let program = constTransformProgram builtin program in
--       -- eprintln "Done const transforming program";
--       -- eprintln "Composing program";
--       -- let program = composeProgram program in
--       -- eprintln "Done composing program";

--       -- -- Todo: symbolizeMLang is currently crashing while encountering undefined language fragments
--       -- -- match symbolizeMLang symEnvDefault program with (symEnv, program) in
--       -- eprintln "Checking composition";
--       -- match result.consume (checkComposition program) with (_, res) in
--       -- eprintln "Done checking composition";

--       -- switch res 
--       --   case Left errs then 
--       --     -- Todo: remove and report errors
--       --     eprintln "TODO: We reached a never in `compileMLang`";
--       --     iter raiseError errs ;
--       --     never
--       --   case Right env then
--       --     -- let ctx = _emptyCompilationContext env in
--       --     -- let res = result.consume (compile ctx program) in
--       --     -- match res with (_, rhs) in
--       --     -- match rhs with Right expr in
--       --     -- let expr = postprocess env.semSymMap expr in
--       --     -- result.ok (program, symEnv)
--       --     result.ok (program, symEnvDefault)
--       -- end

--       result.ok (program, symEnvDefault)
--     in

--     result.bind parseResult handleProgram
-- end

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

    -- let dirtyPaths = getDirtyDependencies environment.dependencies (setEmpty cmpString) strippedUri in
    -- eprintln (join ["Dirty paths: ", strJoin ", " (setToSeq dirtyPaths)]);

    -- let pathClean = lam key. lam. not (setMem key dirtyPaths) in
    -- modref mLangEnvironment {
    --   environment with
    --   files = mapMap (
    --     lam file.
    --       {
    --         file with
    --         includeCache = mapFilterWithKey pathClean file.includeCache
    --       }  
    --   ) environment.files
    -- };

    -- let store: String -> MLangFile -> () = lam path. lam file.
    --   let path = normalizeFilePath path in
    --   let environment = deref mLangEnvironment in
    --   modref mLangEnvironment {
    --     environment with
    --     files = mapInsert path file environment.files
    --   };
    --   ()
    -- in

    -- let load = lam info. lam path.
    --   let path = normalizeFilePath path in
    --   let environment = deref mLangEnvironment in
    --   match mapLookup path environment.files with Some file then
    --     result.ok file
    --   else
    --     let exists = fileExists path in
    --     if not exists then result.err (info, "File not found!") else
    --     match optionMap fileReadString (fileReadOpen path) with Some content in
    --     let file: MLangFile = {
    --       content = content,
    --       includeCache = mapEmpty cmpString,
    --       parsed = None (),
    --       symEnv = None ()
    --     } in
    --     store path file;
    --     result.ok file
    -- in

    -- let addDependency = lam parent. lam child.
    --   let parent = normalizeFilePath parent in
    --   let child = normalizeFilePath child in
    --   let environment = deref mLangEnvironment in
    --   let prev = match mapLookup parent environment.dependencies with Some deps then deps else setEmpty cmpString in
    --   let newDeps = setInsert child prev in

    --   modref mLangEnvironment {
    --     environment with
    --     dependencies = mapInsert parent newDeps environment.dependencies
    --   }
    -- in

    -- let loader: FileLoader = {
    --   load = load,
    --   store = store,
    --   addDependency = addDependency
    -- } in

    -- let currentFile = optionGetOr emptyFile (mapLookup strippedUri environment.files) in

    -- -- Reset/create the file in the environment
    -- loader.store strippedUri {
    --   currentFile with
    --   content = content,
    --   parsed = None (),
    --   symEnv = None ()
    -- };

    -- let compileFile = lam uri.
    --   match result.consume (compileMLang loader uri) with (warnings, compilationResult) in
    --   match compilationResult with Left errors then
    --     mapFromSeq cmpString [
    --       (uri, {
    --         errors = errors,
    --         warnings = warnings,
    --         lookup = lam. lam. None (),
    --         lenses = []
    --       })
    --     ]
    --   else match compilationResult with Right (program, symEnv) in
    --     mapFromSeq cmpString [
    --       (uri, {
    --         errors = [],
    --         warnings = warnings,
    --         lookup = lam. lam. Some ({
    --           info = makeInfo {filename=parameters.uri, row=1, col=1} {filename=parameters.uri, row=1, col=100},
    --           pprint = lam. "Hejsan svejsan",
    --           lookupDefinition = None ()
    --         }),
    --         lenses = []
    --       })
    --   ]
    -- in

    let compileFile = lam uri. lam content.
      use MLangCompilation in 
      let file = parseMLang uri content in

      let includesLookup: [(Info, LookupResult)] =
        match file.kind with Parsed { includes = includes } then
          let f: (Info, Include) -> LookupResult = lam infoInclude.
            match infoInclude with (info, inc) in
            let lookupDefinition = match inc
              with ExistingFile path then Some (lam. makeInfo {filename=path, row=1, col=1} {filename=path, row=1, col=1})
              else None ()
            in
            {
              info = info,
              pprint = lam. use MLangCompilationKind in inc2str inc,
              lookupDefinition = lookupDefinition
            }
          in
          map (lam v. (v.0, f v)) includes
        else 
          []
      in

      let lookup = lam row. lam col.
        recursive let findInclude: [(Info, LookupResult)] -> Option LookupResult =
          lam xs.
            match xs with [x] ++ xs then
              match x with (info, lookupResult) in
              if infoCollision info uri row col then
                Some lookupResult
              else
                findInclude xs
            else
              None ()
        in
        findInclude includesLookup
      in

      let lenses =
        let parsed = match file.kind with Parsed { parsed = parsed } then Some parsed else None () in

        let getDeclLens: Decl -> Option CodeLens = lam decl.
          match decl with DeclUtest { info = info & Info r } then
            Some {
              title = "Run Test",
              ideCommand = "mcore.debugSingle",
              arguments = [
                JsonString r.filename,
                JsonString (info2str info)
              ],
              data = jsonKeyObject [
                ("customData", JsonString "A data entry field that is preserved on a code lens item between a code lens and a code lens resolve request.")
              ],
              location = info
            }
          else
            None ()
        in

        recursive let getExprLens: use MExprAst in Expr -> [CodeLens] =
          lam expr.
            use MExprAst in
        
            let arr = switch expr
              case TmUtest { info = info & Info r } then
                [{
                  title = "Run Test",
                  ideCommand = "mcore.debugSingle",
                  arguments = [
                    JsonString r.filename,
                    JsonString (info2str info)
                  ],
                  data = jsonKeyObject [
                    ("customData", JsonString "A data entry field that is preserved on a code lens item between a code lens and a code lens resolve request.")
                  ],
                  location = info
                }]
              case _ then
                []
            end in
        
            sfold_Expr_Expr (lam acc. lam e.
              let children = getExprLens e in
              concat acc children
            ) arr expr
        in

        let res = optionMap (
          lam parsed.
            join [
              filterOption (map getDeclLens parsed.decls),
              getExprLens parsed.expr
            ]
        ) parsed in

        optionGetOr [] res
      in

      mapFromSeq cmpString [
        (uri, {
          errors = file.errors,
          warnings = file.warnings,
          lookup = lookup,
          lenses = lenses
        })
      ]
    in

    compileFile strippedUri content
    
    -- let paths = setToSeq (setOfSeq cmpString (join [[strippedUri], setToSeq dirtyPaths])) in
    -- eprintln (join ["Compiling files: ", join (map (lam path. join ["'", path, "', "]) paths)]);
    
    -- foldl (lam acc. lam path. mapUnion acc (compileFile path)) (mapEmpty cmpString) paths



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
    pruneMessages = true
  }
} in

eprintln "Miking MCore LSP started";
startLSPServer lspStartParameters;
eprintln "Miking MCore LSP ended"