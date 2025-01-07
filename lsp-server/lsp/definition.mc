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
      jsonKeyObject [
        ("jsonrpc", JsonString "2.0"),
        ("id", JsonInt id),
        ("result", jsonKeyObject [
          ("uri", JsonString r.filename),
          range
        ])
      ]
    else
      jsonKeyObject [
        ("jsonrpc", JsonString "2.0"),
        ("id", JsonInt id),
        ("result", JsonNull ())
      ]

  sem execute context =
  | GotoDefinition { id = id, textDocument = {
    uri = uri,
    line = line,
    character = character
  } } -> 
    -- Add 1 to incoming line and character to match the 1-based indexing of LSP
    let line = addi line 1 in
    let uri = stripUriProtocol uri in

    let environment = mapLookup uri context.environment.files in
    let lookupResult = optionBind environment (lam environment. environment.lookup line character) in
    let lookupDefinition = optionBind lookupResult (lam lookupResult. lookupResult.lookupDefinition) in
    let definition = optionBind lookupDefinition (lam lookupDefinition. lookupDefinition ()) in
    let response = getLSPResponse context id definition in

    {
      response = Some response,
      environment = context.environment
    }

    -- TODO: if definition and variable overlap,
    -- truncate the definition position to end
    -- at the start of the variable position.
    -- Otherwise, LSP will return no result.
    
end