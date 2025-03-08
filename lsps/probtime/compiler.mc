include "../../lsp-server/lsp/root.mc"

lang ProbTimeCompiler = LSPRoot
  syn EventType =
  | Open
  | Change

  -- sem createReversedDependencies : Map URI (Set URI) -> Map URI (Set URI)
  -- sem createReversedDependencies =| dependencies ->
  --   -- Convert the map to a sequence
  --   let seq: [(Path, Set Path)] = mapToSeq dependencies in
  --   -- Create a list of pairs (childUri, parentUri)
  --   let pairs: [(Path, Path)] = flatMap (
  --     lam dependency.
  --       match dependency with (childUri, parentUris) in
  --       map (lam parentUri. (childUri, parentUri)) (setToSeq parentUris)
  --   ) seq in
    
  --   -- Group the pairs by the parentUri
  --   let flatDependencyGraph: Map URI (Set URI) = foldl (
  --     lam acc. lam dependency.
  --       match dependency with (childUri, parentUri) in
  --       mapInsertWith setUnion parentUri (setSingleton cmpString childUri) acc
  --   ) (mapEmpty cmpString) pairs in

  --   -- TODO: Implement parentUri having all children
  --   let reversedDependencies: Map URI (Set URI) = flatDependencyGraph in
  --   reversedDependencies

  -- sem createEmptyFileFromDisk : Path -> MLangFile
  -- sem createEmptyFileFromDisk =| path ->
  --   match optionMap fileReadString (fileReadOpen path) with Some content in
  --   createEmptyFile path content

  sem createFileLoader: EventType -> (LSPCompilationParameters -> LSPCompilationResult)
  sem createFileLoader =| eventType ->
    lam. mapEmpty cmpString

  -- sem compileMLangLSP: (Path -> Option MLangFile) -> MLangFile -> Path -> String -> MLangFile
  -- sem compileMLangLSP getFile file uri =| content ->
  --   let parsed = match (file.parsed, file.status)
  --     with (Some parsed, !Changed ()) then parsed
  --     else lsParseMLang file.filename file.content
  --   in

  --   let linked = match (file.linked, file.status)
  --     with (Some linked, Linked () | Symbolized ()) then linked
  --     else lsLinkMLang file.filename parsed.includes parsed.program
  --   in

  --   let symbolized = match (file.symbolized, file.status)
  --     with (Some symbolized, Symbolized ()) then symbolized
  --     else lsSymbolizeMLang getFile file.filename linked.links linked.program
  --   in

  --   let typeChecked = match (file.typeChecked, file.status)
  --     with (Some typeChecked, TypeChecked ()) then typeChecked
  --     else lsCompileMLangToMExpr getFile file.filename linked.links symbolized.program
  --   in

  --   let file: MLangFile = {
  --     file with
  --     status = TypeChecked (),
  --     -- status = Symbolized (),
  --     parsed = Some parsed,
  --     linked = Some linked,
  --     symbolized = Some symbolized,
  --     typeChecked = Some typeChecked
  --   } in

  --   file
end