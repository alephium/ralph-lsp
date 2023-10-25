import {ExtensionContext, workspace} from 'vscode';

import {LanguageClient, LanguageClientOptions, ServerOptions} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    console.log("Activating Ralph LSP client");

    const args: string[] = ["-jar", "{project-path}/ralph-lsp/lsp-server/target/scala-2.13/ralph-lsp.jar"];

    const serverOptions: ServerOptions = {
        command: "java",
        args: args
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{pattern: '**/*.{ral,ralph}'}],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/')
        }
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
