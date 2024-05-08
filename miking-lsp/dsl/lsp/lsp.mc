include "./unknown.mc"
include "./initialize.mc"
include "./diagnostics.mc"
include "./completion.mc"
include "./hover.mc"
include "./definition.mc"

lang LSP =
LSPInitialize
+ LSPDiagnostics
+ LSPCompletion
+ LSPHover
+ LSPUnknownMethod
+ LSPGotoDefinition
end