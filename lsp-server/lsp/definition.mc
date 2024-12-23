include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"

lang LSPGotoDefinition = LSPRoot
  syn Message =
  | GotoDefinition {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

  sem getMessage request =
  | "textDocument/definition" ->
    match request.id with Some id in
    GotoDefinition {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

  sem getDefinition environment =
  | (_info, variable_name, _ty) ->
    environment.findDefinition variable_name

  sem getLSPResponse context id =
  | definition ->
    match (definition, optionBind definition infoToRange) with (Some (Info r), Some range) then
      let filename = r.filename in

      Some(
        jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt id),
          ("result", jsonKeyObject [
            ("uri", JsonString filename),
            range
          ])
        ]
      )
    else
      Some(
        jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt id),
          ("result", JsonNull ())
        ]
      )

  sem execute context =
  | GotoDefinition { id = id, textDocument = {
    uri = uri,
    line = line,
    character = character
  } } -> 
    -- let strippedUri = "/mnt/ProbTime/examples/coin/coin.rpl" in

    -- Add 1 to incoming line and character to match the 1-based indexing of LSP
    -- let line = addi line 1 in
    -- let strippedUri = stripUriProtocol uri in

    -- let environment = mapLookup uri context.environment.files in
    -- match environment with Some environment then
    --   let variable = environment.findVariable uri line character in
    --   let definition = optionBind variable (getDefinition environment) in
    --   let response = getLSPResponse context id definition in

    --   -- TODO: if definition and variable overlap,
    --   -- truncate the definition position to end
    --   -- at the start of the variable position.
    --   -- Otherwise, LSP will return no result.
      
    --   {
    --     response = response,
    --     environment = context.environment
    --   }
    -- else
      {
        response = None (),
        environment = context.environment
      }

end