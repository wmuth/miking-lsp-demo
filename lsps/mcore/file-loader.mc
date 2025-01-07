include "./include-handler.mc"

lang MLangFileLoader = MLangFileHandler
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

    let load = lam path.
      let path = normalizeFilePath path in
      let environment = deref mLangEnvironment in
      match mapLookup path environment.files with Some file then
        file
      else
        let exists = fileExists path in
        if not exists then error "File not found!" else
        match optionMap fileReadString (fileReadOpen path) with Some content in
        let file = CLoaded { content = content } in
        store path file;
        file
    in

    let loader: FileLoader = {
      load = load,
      store = store
    } in

    loader
end