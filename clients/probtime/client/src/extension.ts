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

function activateStatusBar({ subscriptions }: ExtensionContext) {
  const commandId = "probtime.testCommand";

  subscriptions.push(
    commands.registerCommand(commandId, async () => {
      if (clientActive) {
        let error;
        for (let i = 0; i < 5; i++) {
          try {
            await client.stop();
            window.showInformationMessage(`Stopped ProbTime Language Server`);
            clientActive = false;
            statusBar.text = "$(circle-outline) Start ProbTime LSP";
            return;
          } catch (e) {
            error = e;
          }
        }
        throw new Error(
          `Error stopping ProbTime Language Server: ${error.message}`
        );
      } else {
        try {
          await client.start();
          window.showInformationMessage(`Started ProbTime Language Server`);
          clientActive = true;
          statusBar.text = "$(check) Stop ProbTime LSP";
        } catch (e) {
          window.showErrorMessage(
            `Error starting ProbTime Language Server: ${e.message}`
          );
        }
      }
    })
  );

  statusBar = window.createStatusBarItem(StatusBarAlignment.Left, 100);
  statusBar.command = commandId;
  statusBar.text = "$(check) Stop ProbTime LSP";
  statusBar.show();

  subscriptions.push(statusBar);
}

export async function activate(context: ExtensionContext) {
  const lspServerBin = context.asAbsolutePath(
    path.join("compile-and-start.sh")
  );

  const configuration = workspace.getConfiguration("probtime");

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

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "ProbTime" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  activateStatusBar(context);

  // Create the language client and start the client.
  client = new LanguageClient(
    "probtimeLanguageServer",
    "ProbTime Language Server",
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
