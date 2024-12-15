include "./root.mc"
include "../../lib/pprintjson.mc"

lang LSPMisc = LSPRoot
  syn Message =
  | CancelRequest { id: Int }

  sem getMessage request =
  | "$/cancelRequest" ->
    match mapLookup "id" request.params with Some JsonInt id in
    CancelRequest {
      id = id
    }

  sem execute context =
  | CancelRequest { id = id } ->
    {
      response = Some (jsonKeyObject [
        ("jsonrpc", JsonString "2.0"),
        ("id", JsonInt id),
        ("error", jsonKeyObject [
          ("code", JsonInt -32800),
          ("message", JsonString "Request cancelled")
        ])
      ]),
      environment = context.environment
    }

end