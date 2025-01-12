include "mlang/main.mc"

include "./file.mc"
include "./file-loader.mc"
include "./include-handler.mc"
include "./utests.mc"
include "./main.mc"
include "./dependencies.mc"
include "./upgrade.mc"

include "../../lib/utils.mc"
include "../../lsp-server/lsp/root.mc"

type MLangFile = use MLangFileHandler in MLangFile

type MLangEnvironment = {
  files: Map String MLangFile,
  dependencies: Map String (Set String)
}

type FileLoader =  {
  load: Path -> MLangFile,
  store: Path -> MLangFile -> ()
}

type MLangProgramResult = use MLangAst in Result Diagnostic Diagnostic MLangProgram

lang MLangCompiler =
  LanguageServer +
  MLangAst + MExprAst +
  MLangIncludeHandler + MLangFileLoader +
  MLangFileHandler + MLangUpgradeFile

  sem compile: LSPConfig -> Ref MLangEnvironment -> CompilationParameters -> Map Path [LanguageServerPayload]
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
    -- eprintln (join ["Dirty paths: ", strJoin ", " (setToSeq dirtiedFiles)]);

    let dirtiedFiles = setFilter (lam furi. not (eqString furi uri)) dirtiedFiles in
    let dirtiedFiles = setToSeq dirtiedFiles in

    let loader = createLoader mLangEnvironment in

    -- Downgrade all dependent symbolized files, to trigger
    -- re-symbolization of all dependent files.
    iter (
      lam uri.
        loader.store uri (downgradeSymbolizedFile (loader.load uri))
    ) dirtiedFiles;

    -- Reset/create the file in the environment.
    loader.store uri (CLoaded { content = content, filename = nameSym uri });

    recursive let getFile: Path -> MLangFile = 
      lam uri.
        let file = loader.load uri in
        upgradeFile parameters loader getFile uri file
    in

    let getCompilationResults: [Path] -> [(Path, [LanguageServerPayload])] =
      lam uris.
        let files = map getFile uris in
        map (lam file. (getFilename file, getLanguageSupport file)) files
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

    let file = getFile uri in
    let dependencies = setToSeq (getDependencies getFile (ref (setEmpty cmpString)) file) in
    
    let results = mapFromSeq cmpString (join [
      getCompilationResults [uri],
      getCompilationResults dirtiedFiles,
      getCompilationResults dependencies
    ]) in
    registerDependencies uri;

    -- iter (
    --   lam v.
    --     match v with (uri, file) in
    --       eprintln (join ["File: ", uri, ", status: ", printFileKind file]);
    --       ()
    -- ) (mapToSeq (deref mLangEnvironment).files);

    results
end