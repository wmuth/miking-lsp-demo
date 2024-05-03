include "./unknown.mc"
include "./initialize.mc"
include "./diagnostics.mc"
include "./completion.mc"

lang LSP =
LSPInitialize
+ LSPDiagnostics
+ LSPCompletion
+ LSPUnknownMethod
end