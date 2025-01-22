include "./unknown.mc"
include "./initialize.mc"
include "./change.mc"
include "./completion.mc"
include "./hover.mc"
include "./definition.mc"
include "./misc.mc"
include "./codelens.mc"
include "./implementation.mc"
include "./symbols.mc"

lang LSP =
LSPInitialize
+ LSPUnknownMethod
+ LSPCodeLens
+ LSPChange
+ LSPCompletion
+ LSPHover
+ LSPGotoDefinition
+ LSPImplementation
+ LSPWorkspaceSymbol
+ LSPMisc
end

type MessageWithContext = {
  method: String,
  id: Option Int,
  message: use LSP in Message
}

let getMessage: use LSPRoot in LSPOptions -> String -> Option MessageWithContext =
  lam options. lam body.
    let jsonBody = jsonParseExn body in
    (
      if options.printClientMessages then
        eprintln (join ["Received message: ", body])
      else
        ()
    );
    let request = getRPCRequest jsonBody in
    match request with Some request then
      let method = request.method in
      let id = request.id in
      let message = use LSP in getMessage request request.method in
      Some {
        method = method,
        id = id,
        message = message
      }
    else
      None ()