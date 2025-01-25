/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { mkdtemp } from "fs";
import { tmpdir } from "os";
import * as path from "path";
import {
  workspace,
  ExtensionContext,
  commands,
  tasks,
  Task,
  TaskScope,
  ShellExecution,
  ShellQuoting,
  ShellQuotedString,
  ShellExecutionOptions,
  TaskRevealKind,
  TaskPanelKind,
  TaskGroup,
  StatusBarItem,
  window,
  StatusBarAlignment,
} from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import { LIB_VERSION } from "./version";

let clientActive: boolean = false;
let client: LanguageClient;

let statusBar: StatusBarItem;

async function createTemporaryDirectory() {
  return new Promise<string>((resolve, reject) => {
    mkdtemp(path.join(tmpdir(), "mikinglsp"), (err, directory) => {
      if (err) reject(err);
      else resolve(directory);
    });
  });
}

function createShellTask(
  command: string,
  args: string[],
  source: string,
  {
    quotingStyle = ShellQuoting.Strong,
    ...options
  }: Partial<{
    label: string;
    executionOptions: ShellExecutionOptions;
    quotingStyle: ShellQuoting;
  }>
) {
  const taskCommand: ShellQuotedString = {
    value: command,
    quoting: quotingStyle,
  };
  const taskArgs: ShellQuotedString[] = args.map((arg) => {
    return { value: arg, quoting: quotingStyle };
  });

  const shellExec = new ShellExecution(
    taskCommand,
    taskArgs,
    options.executionOptions
  );

  const label = options.label || command;

  return new Task(
    {
      type: "shell",
      group: "build",
      label,
    },
    TaskScope.Workspace,
    label,
    source,
    shellExec
  );
}

async function runUtest(context: ExtensionContext, url: string, info: string) {
  const target = workspace.workspaceFolders?.[0];
  const output = path.join(await createTemporaryDirectory(), "utest");

  const buildTask = createShellTask(
    context.asAbsolutePath(path.join("run-specific-test.sh")),
    [url, info, output],
    "Miking",
    {
      executionOptions: {
        cwd: target.uri.path,
      },
    }
  );

  buildTask.group = TaskGroup.Build;
  buildTask.presentationOptions = {
    reveal: TaskRevealKind.Always,
    panel: TaskPanelKind.Shared,
    clear: true,
  };

  await tasks.executeTask(buildTask);
}

function activateStatusBar({ subscriptions }: ExtensionContext) {
  const commandId = "mcore.testCommand";

  subscriptions.push(
    commands.registerCommand(commandId, async () => {
      if (clientActive) {
        let error;
        for (let i = 0; i < 5; i++) {
          try {
            await client.stop();
            window.showInformationMessage(`Stopped MCore Language Server`);
            clientActive = false;
            statusBar.text = "$(circle-outline) Start MCore LSP";
            return;
          } catch (e) {
            error = e;
          }
        }
        throw new Error(
          `Error stopping MCore Language Server: ${error.message}`
        );
      } else {
        try {
          await client.start();
          window.showInformationMessage(`Started MCore Language Server`);
          clientActive = true;
          statusBar.text = "$(check) Stop MCore LSP";
        } catch (e) {
          window.showErrorMessage(
            `Error starting MCore Language Server: ${e.message}`
          );
        }
      }
    })
  );

  statusBar = window.createStatusBarItem(StatusBarAlignment.Left, 100);
  statusBar.command = commandId;
  statusBar.text = "$(check) Stop MCore LSP";
  statusBar.show();

  subscriptions.push(statusBar);
}

export async function activate(context: ExtensionContext) {
  const lspServerBin = context.asAbsolutePath(
    path.join("compile-and-start.sh")
  );
  
  // const mcoreCompilerBin = context.asAbsolutePath(
  //   path.join("bin", "compile-mcore")
  // );

  // throw new Error(JSON.stringify({
  //   // env: process.env["MCORE_LIBS"]
  //   env: process.env
  // }));

  const configuration = workspace.getConfiguration("mcore");

  // Somehow my environment variables are not getting passed to the server
  const PATH = [
    process.env["PATH"],
    `/Users/${process.env.USER}/.opam/miking-ocaml/bin`,
    `/Users/${process.env.USER}/.local/bin`,
    `/Users/${process.env.USER}/.opam/miking-ocaml/man/man1`
  ].join(':');

  const serverOptions: ServerOptions = {
    command: lspServerBin,
    args: [LIB_VERSION],
    options: {
      env: {
        ...process.env,
        PATH,
        // MCORE_LIBS: "stdlib=/Users/didrik/projects/miking/miking/stdlib:coreppl=/Users/didrik/projects/miking/miking-dppl/coreppl/src",
        MCORE_LIBS:
          configuration.get<string>("mcoreLibs") || process.env["MCORE_LIBS"],
      },
    },
  };

  context.subscriptions.push(
    commands.registerCommand(
      "mcore.debugSingle",
      async (url: string, info: string) => {
        await runUtest(context, url, info);
      }
    )
  );

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "MCore" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  activateStatusBar(context);

  // Create the language client and start the client.
  client = new LanguageClient(
    "mcoreLanguageServer",
    "MCore Language Server",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  await client.start();
  clientActive = true;
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
