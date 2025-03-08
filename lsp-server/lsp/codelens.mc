include "json.mc"

include "../../lib/utils.mc"
include "./utils.mc"

include "./root.mc"

let getUtestLenses = lam lenses.
  eprintln (join ["Lens count: ", int2string (length lenses)]);

  let createLens: use LanguageServer in LSPCodeLens -> Option JsonValue = lam lens.
    match (infoToRange lens.location, lens.location) with (Some range, Info r) then
      Some (jsonKeyObject [
        range,
        ("command", jsonKeyObject [
          ("title", JsonString lens.title),
          ("command", JsonString lens.ideCommand),
          ("arguments", JsonArray lens.arguments)
        ]),
        ("data", optionGetOr (JsonNull ()) lens.data)
      ])
    else
      None ()
  in

  JsonArray (filterOption (map createLens lenses))

lang LSPCodeLens = LSPRoot
  syn Message =
  | CodeLens {
    id: Int,
    uri: String
  }

  sem getMessage request =
  | "textDocument/codeLens" ->
    match request.id with Some id in
    let params = request.params in
    match mapLookup "textDocument" params with Some JsonObject textDocument in
    match mapLookup "uri" textDocument with Some JsonString uri in
    CodeLens {
      id = id,
      uri = uri
    }

  sem execute context =
  | CodeLens { id = id, uri = uri } ->
    let uri = stripUriProtocol uri in
    let environment = mapLookup uri context.environment.files in

    let lenses = optionMap (lam environment. environment.lenses) environment in
    let result = optionMap getUtestLenses lenses in
    let result = optionGetOr (JsonNull ()) result in

    {
      environment = context.environment,
      response = Some(jsonKeyObject [
        ("jsonrpc", JsonString "2.0"),
        ("id", JsonInt id),
        ("result", result)
      ])
    }

end