include "json.mc"

include "../../lib/utils.mc"
include "./root.mc"

lang LSPInitialize = LSPRoot
  syn Message =
  | Initialized {}
  | Initialize {
    rootUri: Option String
  }

  sem getMessage request =
  | "initialized" ->
    Initialized {}
  | "initialize" ->
    let rootUri = optionMap
      (lam rootUri. match rootUri with JsonString rootUri in rootUri)
      (mapLookup "rootUri" request.params) in

    Initialize {
      rootUri = rootUri
    }

  sem execute context =
  | Initialized {} ->
    
    {
      environment = context.environment,
      response = None ()
    }
  | Initialize {
    rootUri = rootUri
  } ->
    {
      environment = {
        context.environment with
        rootUri = rootUri
      },
      response = Some (
        jsonKeyObject [
          ("jsonrpc", JsonString "2.0"),
          ("id", JsonInt 0),
          ("result", jsonKeyObject [
            ("capabilities", jsonKeyObject [
              ("diagnosticProvider", jsonKeyObject [
                ("interFileDependencies", JsonBool false),
                ("workspaceDiagnostics", JsonBool false)
              ]),
              ("codeLensProvider", jsonKeyObject [
                ("resolveProvider", JsonBool true)
              ]),
              ("hoverProvider", JsonBool true),
              ("textDocumentSync", JsonInt 1),
              ("definitionProvider", JsonBool true),
              ("implementationProvider", JsonBool true),
              ("typeDefinitionProvider", JsonBool true),
              ("completionProvider", jsonKeyObject [
                ("triggerCharacters", JsonArray [
                  JsonString "."
                ])
              ])
            ]),
            ("serverInfo", jsonKeyObject [
              ("name", JsonString "mcoreLanguageServer"),
              ("version", JsonString "0.1.0")
            ])
          ])
        ]
      )
    }
end
