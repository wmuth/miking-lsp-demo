include "json.mc"
include "mexpr/pprint.mc"

include "../../lib/utils.mc"
include "./utils.mc"

include "./root.mc"

lang SuperPrettyPrint = MExprPrettyPrint
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

  type HoverInformation = {
    location: Info,
    toString: [() -> Option String]
  }

  -- Todo: create middleware which bakes the linear information into a searchable tree
  sem findHoverLinearly: URI -> LanguageServerContext -> Int -> Int -> Option HoverInformation
  sem findHoverLinearly uri context line =| character ->
    let hovers = mapToSeq context.hover in

    let foundHovers = filterMap (
      lam hover.
        match hover with (info, toString) in
        if infoCollision info uri line character
          then Some { location = info, toString = toString }
          else None ()
    ) hovers in

    match foundHovers with [first] ++ rest then
      let f = lam hover1. lam hover2.
        if infoContainsInfo hover1.location hover2.location then hover1 else hover2
      in

      Some (foldl f first rest)
    else
      None()

  sem execute context =
    | Hover { id = id, textDocument = {
      uri = uri,
      line = line,
      character = character
    } } ->
      let environment = mapLookup uri context.environment.files in

      -- Add 1 to incoming line and character to match the 1-based indexing of LSP
      let line = addi line 1 in
      let uri = stripUriProtocol uri in

      let lookupResult = optionBind environment (lam environment. findHoverLinearly uri environment line character) in

      let res = optionMap (
        lam lookupResult.
          let contentToJsonString = lam content. JsonString content in

          let info = getFileInfo lookupResult.location in

          let contents = filterMap (lam toString. toString ()) lookupResult.toString in
          let contents = JsonArray (map contentToJsonString contents) in

          jsonKeyObject [
            ("contents", contents),
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
      ) lookupResult in

      let response = optionGetOrElse (lam. JsonNull ()) res in

      {
        environment = context.environment,
        response = Some(jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt id),
          ("result", response)
        ])
      }
end