import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	let config = workspace.getConfiguration('homer');
	let serverExecutable = config.get('serverExecutable') as string;
	let serverOptions: ServerOptions = {
		command: serverExecutable,
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'homer' }],
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'homer',
		'Homer Language Server',
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
