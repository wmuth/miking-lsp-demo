include "./file.mc"
include "./symbolize.mc"
include "./link.mc"
include "./parser.mc"

lang MLangUpgradeFile = MLangFileHandler + MLangSymbolize + MLangLink + MLangParser
  sem upgradeFileInner: FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFile
  sem upgradeFileInner loader getFile path =
  | file -> file
  | file & CLoaded { content = content } ->
    let file = parseMLang file path content in
    upgradeFile loader getFile path file
  | file & CParsed parsed ->
    let file = linkMLang getFile file in
    upgradeFile loader getFile path file
  | file & CLinked { links = links } ->
    let linkedFiles = map (lam x. x.2) links in
    let file = symbolizeMLang path linkedFiles getFile file in
    upgradeFile loader getFile path file

  sem upgradeFile : FileLoader -> (Path -> MLangFile) -> Path -> MLangFile -> MLangFile
  sem upgradeFile loader getFile path =
  | file ->
    let file = upgradeFileInner loader getFile path file in
    loader.store path file;
    file

  sem downgradeSymbolizedFile : MLangFile -> MLangFile
  sem downgradeSymbolizedFile =
  | file -> file
  | CLinked { parsed = parsed } -> CParsed parsed
  | CSymbolized { linked = linked } -> downgradeSymbolizedFile (CLinked linked)
end