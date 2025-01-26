include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"
include "./completion-kind.mc"

-- let getCompletionItems = lam environment.
--   let getCompletionItem = lam definition.
--     match definition with (name, info) in
--     let name = nameGetStr name in
--     jsonKeyObject [
--       ("label", JsonString name),
--       ("kind", JsonInt (use LSPCompletionKind in getCompletionItemKind (CompletionMethod ()))),
--       ("insertText", JsonString (join [name, " in"])),
--       ("insertTextFormat", JsonInt 2),
--       ("documentation", jsonKeyObject [
--         ("kind", JsonString "markdown"),
--         ("value", JsonString "Addition operation `markdown test`")
--       ]),
--       ("deprecated", JsonBool false)
--     ]
--   in

--   map getCompletionItem (mapToSeq environment.definitions)

let getCompletionItem = lam completion: use LSPRoot in Completion.
  jsonKeyObject (filterOption [
    Some ("label", JsonString completion.label),
    Some ("kind", JsonInt (use LSPCompletionKind in getCompletionItemKind completion.kind)),
    Some ("insertText", JsonString (optionGetOr completion.label completion.insertText)),
    Some ("insertTextFormat", JsonInt 1), -- Plain Text
    optionMap (lam documentation. ("documentation", jsonKeyObject [
      ("kind", JsonString "markdown"),
      ("value", JsonString documentation)
    ])) completion.documentation,
    Some ("deprecated", JsonBool completion.deprecated)
  ])

lang LSPCompletion = LSPRoot + LSPCompletionKind
  syn Message =
  | Completion {
    id: Int,
    textDocument: TextDocumentPositionParams
  }

  sem getMessage request =
  | "textDocument/completion" ->
    match request.id with Some id in
    Completion {
      id = id,
      textDocument = getTextDocumentPositionParams request.params
    }

  sem execute context =
    | Completion {
      id = id,
      textDocument = {
        uri = uri,
        line = line,
        character = character
      }
    } ->
      let line = addi line 1 in
      let uri = stripUriProtocol uri in

      let environment = mapLookup uri context.environment.files in

      let findInfo = lam environment. findInfo environment.completions uri line character in
      let getScopes = lam value. match value with (info, generateScopes) in
        foldl (lam acc. lam generateScope. join [acc, generateScope ()]) [] generateScopes
      in

      let generateScopes = optionBind environment findInfo in
      let scopes = optionMap getScopes generateScopes in

      let items = optionMap (map getCompletionItem) scopes in
      let items = optionGetOr [] items in

      {
        environment = context.environment,
        response = Some(
          jsonKeyObject [
            ("jsonrpc", JsonString "2.0"),
            ("id", JsonInt id),
            ("result", jsonKeyObject [
              ("isIncomplete", JsonBool true),
              ("items", JsonArray items)
            ])
          ]
        )
      }
end