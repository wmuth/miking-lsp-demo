include "./file.mc"
include "./symbolize.mc"
include "./link.mc"
include "./parser.mc"

lang MLangUpgradeFile = MLangFileHandler + MLangSymbolize + MLangLink + MLangParser
  sem upgradeFileInner: CompilationParameters -> FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFile
  sem upgradeFileInner parameters loader getFile path =
  | file -> file
  | CLoaded { content = content } ->
    let file = parseMLang path content in
    upgradeFile parameters loader getFile path file
  | file & CParsed parsed ->
    let file = linkMLang getFile file in
    upgradeFile parameters loader getFile path file
  | file & CLinked { links = links } ->
    let linkedFiles = map (lam x. x.2) links in
    let file = symbolizeMLang path linkedFiles file in
    
    upgradeFile parameters loader getFile path file

  sem upgradeFile : CompilationParameters -> FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFile
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
end