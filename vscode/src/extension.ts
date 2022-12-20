import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // The server is implemented in node
  let serverModule = context.asAbsolutePath(path.join('..', 'target', 'debug', 'bytec'));

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions: ServerOptions = {
    run: {
      command: serverModule,
      args: ["server"],
      options: {
        env: {
          "RUST_BACKTRACE": 1,
        }
      }
    },
    debug: {
      command: serverModule,
      args: ["server"],
      options: {
        env: {
          "RUST_BACKTRACE": 1,
        }
      }
    }
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for Bytec files
    documentSelector: [{ scheme: 'file', language: 'bytec' }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'bytecLanguageServer',
    'ByteC Language Server',
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}