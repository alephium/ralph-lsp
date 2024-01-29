import {ExtensionContext, workspace} from 'vscode';
import * as vscode from 'vscode'
import * as fs from 'fs'

import {LanguageClient, LanguageClientOptions, ServerOptions} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    console.log("Activating Ralph LSP client");

    const config =  workspace.getConfiguration('ralphLSP')
    const jarPath = config.get('server.jar') as string

    if(fs.existsSync(jarPath)) {
        const args: string[] = ["-jar", "-DRALPH_LSP_HOME=<path-to-your-log-folder>", jarPath]
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
    } else {
        vscode.window
        .showErrorMessage(`${jarPath} doesn't exist` , 'Open setting')
        .then(_ => { vscode.commands.executeCommand("workbench.action.openSettings", "ralphLSP.server.jar")})
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
