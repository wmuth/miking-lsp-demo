include "root.mc"

let mini: Int -> Int -> Int =
  lam a. lam b. if leqi a b then a else b

let maxi: Int -> Int -> Int =
  lam a. lam b. if leqi a b then b else a

lang LSPDiagnosticSeverity
  -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
  syn DiagnosticSeverity =
    | Error
    | Warning
    | Information
    | Hint

  type DiagnosticWithSeverity = (Info, String, DiagnosticSeverity)

  sem decorateDiagnosticWithSeverity: DiagnosticSeverity -> (Info, String) -> DiagnosticWithSeverity
  sem decorateDiagnosticWithSeverity =
  | severity -> lam diagnostic. (diagnostic.0, diagnostic.1, severity)

  sem getDiagnosticSeverity: DiagnosticSeverity -> Int
  sem getDiagnosticSeverity =
  | Error () -> 1
  | Warning () -> 2
  | Information () -> 3
  | Hint () -> 4
end

let getDiagnosticsObject: String -> use LSPDiagnosticSeverity in DiagnosticWithSeverity -> JsonValue =
  lam source. lam diagnostic.
    let info = diagnostic.0 in
    let msg = diagnostic.1 in
    let severity = diagnostic.2 in

    let fileInfo = getFileInfo info in
    let uri = fileInfo.filename in

    jsonKeyObject [
      ("message", JsonString msg),
      ("severity", JsonInt (use LSPDiagnosticSeverity in getDiagnosticSeverity severity)),
      ("source", JsonString source),
      ("range", jsonKeyObject [
        ("start", jsonKeyObject [
          ("line", JsonInt (subi fileInfo.lineStart 1)),
          ("character", JsonInt fileInfo.colStart)
        ]),
        ("end", jsonKeyObject [
          ("line", JsonInt (subi fileInfo.lineEnd 1)),
          ("character", JsonInt fileInfo.colEnd)
        ])
      ])
    ]

let getDiagnostic: String -> String -> [use LSPDiagnosticSeverity in DiagnosticWithSeverity] -> JsonValue =
  lam source. lam uri. lam diagnostics.
    let diagnostics = map (getDiagnosticsObject source) diagnostics in

    jsonKeyObject [
      ("jsonrpc", JsonString "2.0"),
      ("method", JsonString "textDocument/publishDiagnostics"),
      ("params", jsonKeyObject [
        ("uri", JsonString uri),
        ("diagnostics", JsonArray diagnostics)
      ])
    ]

let getDiagnostics: String -> Map URI [use LSPDiagnosticSeverity in DiagnosticWithSeverity] -> [JsonValue] =
  lam source. lam diagnosticsGroupedByFile.
    let createDiagnosticsFromGroupedDiagnostics = lam groupedDiagnostic.
      let uri = groupedDiagnostic.0 in
      let diagnostics = groupedDiagnostic.1 in
      getDiagnostic source uri diagnostics
    in
    
    map createDiagnosticsFromGroupedDiagnostics (mapToSeq diagnosticsGroupedByFile)

let getDiagnosticFileName =
  lam diagnostic.
    (getFileInfo diagnostic.0).filename

let getResultResponses: URI -> use LSPRoot in LanguageServerContext -> [JsonValue] =
  lam uri. lam compilationResult.
    -- let extractDiagnostics = lam f.
    --   foldl
    --     (lam acc: [(Info, String)]. lam diagnostic: [(Info, String)]. join [acc, diagnostic])
    --     []
    --     (map f (mapValues compilationResults))
    -- in

    -- let errors = extractDiagnostics (lam compilationResult. compilationResult.errors) in
    -- let warnings = extractDiagnostics (lam compilationResult. compilationResult.warnings) in

    let errors = compilationResult.errors in
    let warnings = compilationResult.warnings in

    use LSPDiagnosticSeverity in
    let errors = map (decorateDiagnosticWithSeverity (Error ())) errors in
    let warnings = map (decorateDiagnosticWithSeverity (Warning ())) warnings in

    let diagnostics = join [errors, warnings] in
    -- let diagnosticsGroupedByFile = groupBy cmpString getDiagnosticFileName diagnostics in

    -- -- If there are no errors for this file, we need to include an empty list to reset the errors in the IDE.
    -- let emptyDiagnosticsForThisFile = mapFromSeq cmpString (map (lam uri. (uri, [])) (mapKeys compilationResults)) in
    -- let diagnosticsGroupedByFile = mapUnionWith concat diagnosticsGroupedByFile emptyDiagnosticsForThisFile in

    let diagnosticsGroupedByFile = mapInsert uri diagnostics (mapEmpty cmpString) in

    getDiagnostics "MCore" diagnosticsGroupedByFile