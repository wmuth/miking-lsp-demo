include "./file.mc"
include "./symbolize.mc"
include "./parser.mc"

lang MLangUpgradeFile = MLangFileHandler + MLangSymbolize + MLangParser
  sem upgradeFile: Path -> MLangFile -> MLangFile
  sem upgradeFile path =
  | file -> file
  | file & CLoaded { content = content } ->
    let file = parseMLang file path content in
    upgradeFile path file
  | file & CParsed parsed ->
    let file = symbolizeMLangLanguageSupport path file in
    upgradeFile path file

  sem downgradeSymbolizedFile : MLangFile -> MLangFile
  sem downgradeSymbolizedFile =
  | file -> file
  | CSymbolized { parsed = parsed } -> downgradeSymbolizedFile (CParsed parsed)
end