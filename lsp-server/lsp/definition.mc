include "json.mc"

include "../../lib/utils.mc"
include "./utils.mc"

include "./root.mc"
include "../mexpr.mc"

lang LSPGotoDefinition = LSPRoot
syn Params =
  | GotoDefinition {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

sem getParams request =
  | "textDocument/definition" ->
    match request.id with Some id in
    GotoDefinition {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

sem getDefinition environment =
  | (_info, variable_name, _ty) ->
    -- (
    -- 	match variable with Some (info, variable) then
    -- 		eprintln (join ["Found variable: ", info2str info]); ()
    -- 	else
    -- 		eprintln "No variable found"; ()
    -- );

    environment.findDefinition variable_name

sem getLSPResponse context id =
  | definition ->
    match definition with Some info then
      -- eprintln (join ["Found definition: ", info2str info]);

      let info = getFileInfo info in

      -- let filename = match info.filename with "/app/" ++ rest then
      -- 	join ["/Users/didrik/projects/miking/", rest]
      -- else 
      -- 	originalUri
      -- in

      let filename = info.filename in 

      eprintln (join ["Going to: ", filename, ":", int2string info.lineStart, ":", int2string info.colStart, "-", int2string info.lineEnd, ":", int2string info.colEnd]);

      Some(
        jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt id),
          ("result", jsonKeyObject [
            ("uri", JsonString filename),
            ("range", jsonKeyObject [
              ("start", jsonKeyObject [
                ("line", JsonInt (subi info.lineStart 1)),
                ("character", JsonInt info.colStart)
              ]),
              ("end", jsonKeyObject [
                ("line", JsonInt (subi info.lineEnd 1)),
                ("character", JsonInt info.colEnd)
              ])
            ])
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
    let line = addi line 1 in
    let strippedUri = stripUriProtocol uri in

    let environment = mapLookup uri context.environment.files in
    match environment with Some environment then
      let variable = environment.findVariable uri line character in
      let definition = optionBind variable (getDefinition environment) in
      let response = getLSPResponse context id definition in

      -- TODO: if definition and variable overlap,
      -- truncate the definition position to end
      -- at the start of the variable position.
      -- Otherwise, LSP will return no result.
      
      {
        response = response,
        environment = context.environment
      }
    else
      {
        response = None (),
        environment = context.environment
      }

end