import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	TextDocumentSyncKind,
	InitializeResult
} from 'vscode-languageserver/node';

import {
	WorkspaceFolder
} from "vscode-languageserver-protocol"

import {AquaLSP} from "../js/compiler";

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasWorkspaceFolderCapability = false;
let folders: WorkspaceFolder[] = []

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);

	if (params.workspaceFolders) {
		folders = params.workspaceFolders
	}

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	connection.workspace.onDidChangeWorkspaceFolders(_event => {
		connection.console.log('Workspace folder change event received.');
	});

});

documents.onDidSave(async change => {
	await validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {

	const uri = textDocument.uri.replace("file://", "")

	let imports: string[] = []

	// add all workspace folders to imports
	imports = imports.concat(folders.map((f) => f.uri.replace("file://", "")))
	imports = imports.concat(folders.map((f) => f.uri.replace("file://", "")) + "/node_modules")

	if (require.main) {
		imports = imports.concat(require.main.paths)
	}

	const errors = await AquaLSP.compile(uri, imports)

	const diagnostics: Diagnostic[] = [];

	if (errors) {
		// Add all errors to Diagnostic
		errors.forEach((err) => {
			const diagnostic: Diagnostic = {
				severity: DiagnosticSeverity.Error,
				range: {
					start: textDocument.positionAt(err.start),
					end: textDocument.positionAt(err.end)
				},
				message: err.message
			};

			if (err.location) {
				diagnostic.relatedInformation = [
					{
						location: {
							uri: err.location,
							range: Object.assign({}, diagnostic.range)
						},
						message: 'Compilation error'
					}
				];
			}


			diagnostics.push(diagnostic);
		})
	}

	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
