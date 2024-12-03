include "./unknown.mc"
include "./initialize.mc"
include "./change.mc"
include "./completion.mc"
include "./hover.mc"
include "./definition.mc"
include "./misc.mc"

lang LSP =
LSPInitialize
+ LSPUnknownMethod
+ LSPChange
-- + LSPCompletion
+ LSPHover
+ LSPGotoDefinition
+ LSPMisc
end