import {ExtensionContext} from 'vscode';

import {LanguageClient, LanguageClientOptions, ServerOptions} from 'vscode-languageclient/node';
import * as path from "node:path";

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

    const args =
        build_args(true);

    const serverOptions: ServerOptions = {
        command: "java",
        args: args
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            {pattern: '**/*.ral'},
            {language: 'json', pattern: '**/ralph.json'},
        ]
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
