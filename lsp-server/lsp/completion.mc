include "json.mc"

include "../../lib/utils.mc"
include "./root.mc"

lang LSPCompletionItem

  -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
  syn CompletionItemKind =
    | Text
    | Method
    | Function
    | Constructor
    | Field
    | Variable
    | Class
    | Interface
    | Module
    | Property
    | Unit
    | Value
    | Enum
    | Keyword
    | Snippet
    | Color
    | File
    | Reference
    | Folder
    | EnumMember
    | Constant
    | Struct
    | Event
    | Operator
    | TypeParameter

  sem getCompletionItemKind: CompletionItemKind -> Int
  sem getCompletionItemKind =
  | Text () -> 1
  | Method () -> 2
  | Function () -> 3
  | Constructor () -> 4
  | Field () -> 5
  | Variable () -> 6
  | Class () -> 7
  | Interface () -> 8
  | Module () -> 9
  | Property () -> 10
  | Unit () -> 11
  | Value () -> 12
  | Enum () -> 13
  | Keyword () -> 14
  | Snippet () -> 15
  | Color () -> 16
  | File () -> 17
  | Reference () -> 18
  | Folder () -> 19
  | EnumMember () -> 20
  | Constant () -> 21
  | Struct () -> 22
  | Event () -> 23
  | Operator () -> 24
  | TypeParameter () -> 25
end

let getCompletionItems = lam environment.
  let getCompletionItem = lam definition.
    match definition with (name, info) in
    jsonKeyObject [
      ("label", JsonString (nameGetStr name)),
      ("kind", JsonInt (use LSPCompletionItem in getCompletionItemKind (Method ()))),
      ("insertText", JsonString "+"),
      ("insertTextFormat", JsonInt 2),
      ("documentation", jsonKeyObject [
        ("kind", JsonString "markdown"),
        ("value", JsonString "Addition operation `markdown test`")
      ]),
      ("deprecated", JsonBool false)
    ]
  in

  map getCompletionItem (mapToSeq environment.definitionLookup)

lang LSPCompletion = LSPRoot
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
      let environmentMaybe = mapLookup uri context.environment.files in
      let items = optionMap getCompletionItems environmentMaybe in
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

  -- sem execute context =
  --   | Completion { id = id } ->
  --     {
  --       environment = context.environment,
  --       response = Some(
  --         jsonKeyObject [
  --           ("jsonrpc", JsonString "2.0"),
  --           ("id", JsonInt id),
  --           ("result", jsonKeyObject [
  --           ("isIncomplete", JsonBool true),
  --           ("items", JsonArray [
  --             jsonKeyObject [
  --             ("label", JsonString "add"),
  --             ("kind", JsonInt (getCompletionItemKind (Method ()))),
  --             ("insertText", JsonString "+"),
  --             ("insertTextFormat", JsonInt 2),
  --             ("documentation", jsonKeyObject [
  --               ("kind", JsonString "markdown"),
  --               ("value", JsonString "Addition operation `markdown test`")
  --             ]),
  --             ("deprecated", JsonBool true)
  --             ],
  --             jsonKeyObject [
  --             ("label", JsonString "sub"),
  --             ("kind", JsonInt (getCompletionItemKind (Constructor ()))),
  --             ("insertText", JsonString "-"),
  --             ("insertTextFormat", JsonInt 2)
  --             ],
  --             jsonKeyObject [
  --             ("label", JsonString "mul"),
  --             ("kind", JsonInt (getCompletionItemKind (Function ()))),
  --             ("insertText", JsonString "*"),
  --             ("insertTextFormat", JsonInt 2)
  --             ],
  --             jsonKeyObject [
  --             ("label", JsonString "div"),
  --             ("kind", JsonInt (getCompletionItemKind (Class ()))),
  --             ("insertText", JsonString "/"),
  --             ("insertTextFormat", JsonInt 2)
  --             ]
  --           ])
  --           ])
  --         ]
  --       )
  --     }
end