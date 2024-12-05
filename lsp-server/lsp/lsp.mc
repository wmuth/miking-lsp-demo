include "./unknown.mc"
include "./initialize.mc"
include "./change.mc"
include "./completion.mc"
include "./hover.mc"
include "./definition.mc"
include "./misc.mc"
include "./codelens.mc"

lang LSP =
LSPInitialize
+ LSPUnknownMethod
+ LSPCodeLens
+ LSPChange
+ LSPCompletion
+ LSPHover
+ LSPGotoDefinition
+ LSPMisc
end

type MessageWithContext = {
  method: String,
  id: Option Int,
  message: use LSP in Message
}

let getMessage: String -> MessageWithContext =
  lam body.
    let jsonBody = jsonParseExn body in
    let request = getRPCRequest jsonBody in
    let method = request.method in
    let id = request.id in
    let message = use LSP in getMessage request request.method in
    {
      method = method,
      id = id,
      message = message
    }