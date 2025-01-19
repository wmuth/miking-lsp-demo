include "json.mc"
include "fileutils.mc"

include "./utils.mc"
include "../../lib/utils.mc"
include "./root.mc"
include "./compile.mc"

let sysGetWorkspaceFiles : String -> URI -> [URI] =
  lam extension. lam dir.
    let cmd = join ["find ", dir, " -type f -name \"*.", extension, "\""] in
    let res = strTrim (sysRunCommand [cmd] "" dir).stdout in
    let files = strSplit "\n" res in
    files

lang LSPInitialize = LSPRoot + LSPCompileUtility
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
      rootUri = optionMap stripUriProtocol rootUri
    }

  syn ProgressReport =
  | Begin {
    message: String,
    percentage: Int
  }
  | Report {
    message: String,
    percentage: Int
  }
  | End {
    message: String
  }

  -- TODO: Use this
  sem createProgressNotificationPayload : ProgressReport -> JsonValue
  sem createProgressNotificationPayload =
  | Begin { message = message, percentage = percentage } ->
    jsonKeyObject [
      ("kind", JsonString "begin"),
      ("message", JsonString message),
      ("percentage", JsonInt percentage)
    ]
  | Report { message = message, percentage = percentage } ->
    jsonKeyObject [
      ("kind", JsonString "report"),
      ("message", JsonString message),
      ("percentage", JsonInt percentage)
    ]
  | End { message = message } ->
    jsonKeyObject [
      ("kind", JsonString "end"),
      ("message", JsonString message)
    ]

  sem execute context =
  | Initialized {} ->
    let environment = match context.environment.rootUri
      with Some rootUri then
        let workspaceFiles = sysGetWorkspaceFiles context.environment.extension rootUri in
        foldl (
          lam environment. lam uri.
            let context = {
              context with
              environment = environment
            } in

            match optionMap fileReadString (fileReadOpen uri) with Some text in
            handleCompile (Open ()) context uri text context.parameters.onOpen
        ) context.environment workspaceFiles
      else
        context.environment
    in
    
    {
      environment = environment,
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
