include "json.mc"

include "../../lib/utils.mc"

include "./utils.mc"
include "./root.mc"
include "./completion-kind.mc"

let getCompletionItem = lam completion: use LSPRoot in LSPCompletion.
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

      let availableDefinitions = optionMap (lam environment. environment.availability) environment in
      let availableDefinitions = optionGetOr (mapEmpty infoCmp) availableDefinitions in
      let availableDefinitions = mapFilterWithKey (
        lam info. lam names.
          infoCollision info uri line character
      ) availableDefinitions in
      let availableDefinitions = (compose flattenMap mapValues) availableDefinitions in
      let availableDefinitions = setOfSeq nameCmp availableDefinitions in

      -- let info = getFileInfo lookupResult.location in

      -- let findAllInfo = lam environment. findAllInfo environment.completions uri line character in
      -- let generateCompletions: (Info, [() -> Completion]) -> [Completion] = lam value. match value with (info, generateCompletions) in
      --   map (lam generateCompletion. generateCompletion ()) generateCompletions
      -- in
      let getAllSymbols: () -> [LSPCompletion] = lam.
        let definitions = optionMap (lam environment. environment.definitions) environment in
        -- Remove definitions that are not available
        -- let definitions = optionMap (mapFilterWithKey (
        --   lam name. lam definitions.
        --     setMem name availableDefinitions
        -- )) definitions in
        let definitions = optionMap mapToSeq definitions in
        let definitions = optionGetOr [] definitions in

        let mapDefinitions = lam definition.
          match definition with (name, definitions) in
          let definitions = filter (
            lam definition. match definition.kind with SymbolFile () then false else true
          ) definitions in
          map (
            lam definition. {
              label = nameGetStr name,
              kind = symbolToCompletion definition.kind,
              insertText = None (),
              documentation = Some ((compose (strJoin "\n") filterOption) [
                definition.documentation (),
                optionMap (lam location. join [
                  "[`", info2str location, "`]: ",
                  "file://", info2link location, "\n\n"
                ]) definition.location
              ]),
              deprecated = false
            }
          ) definitions
        in

        flatMap mapDefinitions definitions
      in

      -- eprintln (join ["Completion request: ", uri, ":", int2string line, ":", int2string character]);
      -- (
      --   match environment with Some environment then
      --     eprintln (join ["Environment found: ", uri]);
      --     eprintln (join ["Completions: ", strJoin ", " (map (lam v. match v with (info, _) in info2str info) (mapToSeq environment.completions))]);
      --     ()
      --   else ()
      -- );

      -- let items: Option ([(Info, [() -> Completion])]) = optionMap findAllInfo environment in
      -- let items: Option [Completion] = optionMap (flatMap generateCompletions) items in
      -- let items = optionGetOr [] items in
      -- -- let scopes = join [
      -- --   scopes
      -- --   -- getAllSymbols ()
      -- -- ] in

      let items = getAllSymbols () in
      let items = map getCompletionItem items in

      {
        environment = context.environment,
        response = Some(
          jsonKeyObject [
            ("jsonrpc", JsonString "2.0"),
            ("id", JsonInt id),
            ("result", jsonKeyObject [
              ("isIncomplete", JsonBool false),
              ("items", JsonArray items)
            ])
          ]
        )
      }
end