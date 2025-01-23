include "json.mc"
include "fileutils.mc"

include "./utils.mc"
include "../../lib/utils.mc"
include "./root.mc"
include "./compile.mc"
include "./progress.mc"

let sysGetWorkspaceFiles : String -> URI -> [URI] =
  lam extension. lam dir.
    let cmd = join ["find ", dir, " -type f -name \"*.", extension, "\""] in
    let res = strTrim (sysRunCommand [cmd] "" dir).stdout in
    let files = strSplit "\n" res in
    files

lang LSPInitialize =
  LSPRoot + LSPCompileUtility + LSPProgress

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

  sem execute context =
  | Initialized {} ->
    let environment = match (context.environment.options.indexWorkspace, context.environment.rootUri)
      with (true, Some rootUri) then
        eprintln "Indexing workspace files";
        let workspaceFiles = sysGetWorkspaceFiles context.environment.options.extension rootUri in
        let len = length workspaceFiles in
        
          let progress = createProgress context.sendNotification in
          progress.reportMsg 0.0 (join ["Indexing files in workspace: ", rootUri]);

          let environment = foldli (
            lam environment. lam i. lam uri.
              let context = {
                context with
                environment = environment
              } in

              match optionMap fileReadString (fileReadOpen uri) with Some text in
              let res = handleCompile (Open ()) context uri text context.parameters.onOpen in

              let indexedUri = (compose join tail) (strSplit rootUri uri) in
              progress.reportMsg
                (divf (int2float (addi i 1)) (int2float len))
                (join [
                  "Indexing files: ", int2string (addi i 1), "/", int2string len,
                  " (.", indexedUri, ")"
                ]);

              res
          ) context.environment workspaceFiles in

          progress.finish (Some "Finished indexing files");
          environment
      else
        eprintln "Skipping indexing workspace files";
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
              ("workspaceSymbolProvider", JsonBool true),
              ("definitionProvider", JsonBool true),
              ("implementationProvider", JsonBool true),
              ("typeDefinitionProvider", JsonBool true),
              ("typeHierarchyProvider", JsonBool true),
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
