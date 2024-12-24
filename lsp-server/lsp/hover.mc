include "json.mc"
include "mexpr/pprint.mc"

include "../../lib/utils.mc"
include "./utils.mc"

include "./root.mc"
-- include "coreppl::parser.mc"

lang SuperPrettyPrint = MExprPrettyPrint --+ DPPLParser
end

lang LSPHover = LSPRoot
  syn Message =
  | Hover {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

  sem getMessage request =
  | "textDocument/hover" ->
    match request.id with Some id in
    Hover {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

  sem execute context =
    | Hover { id = id, textDocument = {
      uri = uri,
      line = line,
      character = character
    } } -> 
      -- Add 1 to incoming line and character to match the 1-based indexing of LSP
      let line = addi line 1 in
      let uri = stripUriProtocol uri in

      let debugText = join [
        "Uri: ", uri, ", ",
        "Line: ", int2string line, ", Character: ", int2string character, "\n\n"
      ] in

      match mapLookup uri context.environment.files with Some environment then
        let response = match environment.lookup line character with Some lookupResult then
          let info = getFileInfo lookupResult.info in
          let contentToJsonString = lam content. JsonString content in
          let content = optionMap contentToJsonString (lookupResult.pprint ()) in
          let contents = optionGetOrElse (lam. JsonNull ()) content in

          jsonKeyObject [
            (
              "contents", contents
            ),
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
          ]
        else
          eprintln "Variable not found";
          jsonKeyObject [(
            "contents", JsonString (join [
              debugText,
              "[nothing found]"
            ])
          )]
        in
          {
            environment = context.environment,
            response = Some(jsonKeyObject [
              ("jsonrpc", JsonString "2.0"),
              ("id", JsonInt id),
              ("result", response)
            ])
          }
      else
        {
          environment = context.environment,
          response = Some(jsonKeyObject [
            ("jsonrpc", JsonString "2.0"),
            ("id", JsonInt id),
            ("result", JsonNull ())
          ])
        }

end