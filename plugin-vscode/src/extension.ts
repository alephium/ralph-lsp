import {ExtensionContext, workspace} from 'vscode';

import {LanguageClient, LanguageClientOptions, ServerOptions} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    console.log("Activating Ralph LSP client");

    const args: string[] = ["-jar", "-DRALPH_LSP_HOME=<path-to-your-log-folder>", "<path-to-your-jar>/ralph-lsp.jar"];

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
        'ralph-lsp-client',
        'LSP client for Ralph',
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
