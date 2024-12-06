include "json.mc"

include "../../lib/utils.mc"
include "./utils.mc"

include "./root.mc"

let getUtestLenses = lam environment.
  let createLens = lam info.
    match (infoToRange info, info) with (Some range, Info r) then
      Some (jsonKeyObject [
        range,
        ("command", jsonKeyObject [
          ("title", JsonString "Run Test"),
          ("command", JsonString "mcore.debugSingle"),
          ("arguments", JsonArray [
            JsonString (stripTempFileExtension r.filename),
            JsonString (info2str (stripTempFileExtensionFromInfo info))
          ])
        ]),
        ("data", jsonKeyObject [
          ("customData", JsonString "A data entry field that is preserved on a code lens item between a code lens and a code lens resolve request.")
        ])
      ])
    else
      None ()
  in

  JsonArray (filterOption (map createLens environment.utestLookup))

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
      let result = match mapLookup uri context.environment.files
        with Some environment then
          (getUtestLenses environment, context.environment)
        else
          (JsonNull (), context.environment)
      in

      match result with (result, environment) in

      {
        environment = environment,
        response = Some(jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt id),
          ("result", result)
        ])
      }

end