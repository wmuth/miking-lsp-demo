include "./include-handler.mc"
include "./file.mc"

lang MLangFileLoader = MLangFileHandler
  type FileLoader =  {
    load: Path -> MLangFile,
    store: Path -> MLangFile -> (),
    has: Path -> Bool
  }

  sem createLoader: Ref MLangEnvironment -> FileLoader
  sem createLoader =| mLangEnvironment ->
    let store: Path -> MLangFile -> () = lam path. lam file.
      let path = normalizeFilePath path in
      let environment = deref mLangEnvironment in
      modref mLangEnvironment {
        environment with
        files = mapInsert path file environment.files
      };
      ()
    in

    let has = lam path.
      let path = normalizeFilePath path in
      let environment = deref mLangEnvironment in
      optionIsSome (mapLookup path environment.files)
    in

    let load = lam path.
      let path = normalizeFilePath path in
      let environment = deref mLangEnvironment in
      match mapLookup path environment.files with Some file then
        file
      else
        let exists = fileExists path in
        if not exists then error "File not found!" else
        match optionMap fileReadString (fileReadOpen path) with Some content in
        let file = CLoaded { content = content, filename = nameSym path } in
        store path file;
        file
    in

    let loader: FileLoader = {
      load = load,
      store = store,
      has = has
    } in

    loader
end