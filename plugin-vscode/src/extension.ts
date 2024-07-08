import {ExtensionContext, Uri, workspace, } from 'vscode';

import {LanguageClient, LanguageClientOptions, ServerOptions} from 'vscode-languageclient/node';
import * as path from "node:path";
import * as fs from 'fs';


let client: LanguageClient;

/**
 * Build arguments for ralph-lsp.jar initialisation.
 *
 * @param release `true` to build args for publishing, else `false` for development.
 */
function build_args(release: boolean) {
    const dirName =
        __dirname;

    const jarPath =
        path.resolve(dirName, 'ralph-lsp.jar');

    let args: string[];

    if (release) {
        args = ["-jar", jarPath]; // arguments for plugin release
    } else {
        const logsPath = path.resolve(dirName, "../../"); // path for logs
        args = ["-jar", `-DRALPH_LSP_LOG_HOME=${logsPath}`, jarPath]; // arguments for local development
    }

    return args;
}

export function activate(context: ExtensionContext) {
    console.log("Activating Ralph LSP client");

    const rootFiles = ['alephium.config.ts', 'contracts', '.ralph-lsp'];

    const workspaceFolders = workspace.workspaceFolders;

    let rootDir: string | null = null;

    if (workspaceFolders) {
        for (const folder of workspaceFolders) {
            const potentialRootDir = findRootDir(folder.uri.fsPath, rootFiles);
            if (potentialRootDir) {
                rootDir = potentialRootDir;
                break;
            }
        }
    }

    const args =
        build_args(true);

    const serverOptions: ServerOptions = {
        command: "java",
        args: args
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            {pattern: '**/*.ral'},
            {language: 'json', pattern: '**/ralph.json'}
        ],
        workspaceFolder: rootDir ? { uri: Uri.file(rootDir), name: "ralph-lsp-home", index: 0} : undefined
    };

    // Create the client and store it.
    client = new LanguageClient(
        'ralph-lsp',
        'Ralph LSP',
        serverOptions,
        clientOptions
    );

    // Start the client.
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

// Helper function to recursively find the root directory
function findRootDir(currentDir: string, rootFiles: string[]): string | null {

  for (const file of rootFiles) {
    if (fs.existsSync(path.join(currentDir, file))) {
      return currentDir;
    }
  }

  const parentDir = path.dirname(currentDir);
  if (parentDir === currentDir) {
    // Reached the root directory
    return null;
  }

  return findRootDir(parentDir, rootFiles);
}
