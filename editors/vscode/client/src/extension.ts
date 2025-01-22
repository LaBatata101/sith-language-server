import { ExtensionContext, workspace } from "vscode";
import * as vscode from "vscode";
import * as fs from "fs-extra";

import {
    Disposable,
    Executable,
    LanguageClient,
    LanguageClientOptions,
    RevealOutputChannelOn,
    ServerOptions,
} from "vscode-languageclient/node";
import { logger, LazyOutputChannel } from "./utils/logger";
import {
    checkIfConfigurationChanged,
    getExtensionSettings,
    getGlobalSettings,
    getWorkspaceSettings,
} from "./utils/settings";
import { findSithBinaryPath, getProjectRoot } from "./utils/fs";
import { DEV_SITH_BIN_BUILD_PATH } from "./utils/constants";
import { registerCommand } from "./utils/vscodeapi";

let client: LanguageClient | undefined;
let restartInProgress = false;
let restartQueued = false;
const serverId = "sith";

export async function activate(context: ExtensionContext) {
    await runServer();

    context.subscriptions.push(
        workspace.onDidChangeConfiguration(async (e: vscode.ConfigurationChangeEvent) => {
            if (checkIfConfigurationChanged(e, serverId)) {
                await runServer();
            }
        }),
    );
}

export function deactivate(): Thenable<void> {
    if (client) {
        return stopServer();
    }
}

async function runServer() {
    if (restartInProgress) {
        if (!restartQueued) {
            // Schedule a new restart after the current restart.
            logger.trace(`Triggered SithLSP restart while restart is in progress; queuing a restart.`);
            restartQueued = true;
        }
        return;
    }

    restartInProgress = true;

    try {
        if (client) {
            await stopServer();
        }

        await startServer();
    } finally {
        // Ensure that we reset the flag in case of an error, early return, or success.
        restartInProgress = false;
        if (restartQueued) {
            restartQueued = false;
            await runServer();
        }
    }
}

let _disposables: Disposable[] = [];

async function startServer() {
    logger.info("Server started");

      // Create output channels for the server and trace logs
  const outputChannel = vscode.window.createOutputChannel(`Sith Language Server`);
  _disposables.push(outputChannel);
  const traceOutputChannel = new LazyOutputChannel(`Sith Language Server Trace`);
  _disposables.push(traceOutputChannel);
  // And, a command to show the server logs
  _disposables.push(registerCommand(`${serverId}.showServerLogs`, () => outputChannel.show()));

    const extensionSettings = getExtensionSettings(serverId);
    const globalSettings = getGlobalSettings(serverId);

    const projectRoot = await getProjectRoot();
    const workspaceSettings = await getWorkspaceSettings(serverId, projectRoot);
    if (!workspaceSettings.interpreter) {
        logger.error("Python interpreter not found!");
        vscode.window
            .showErrorMessage(
                `Please set a Python interpreter using "${serverId}.interpreter" setting.`,
                "Open Settings",
            )
            .then((selection) => {
                if (selection === "Open Settings") {
                    vscode.commands.executeCommand(
                        "workbench.action.openSettings",
                        "sith.interpreter",
                    );
                }
            });
        return;
    }

    logger.trace(`Using Python interpreter: ${workspaceSettings.interpreter}`);

    if (!fs.statSync(globalSettings.interpreter).isFile()) {
        logger.error("Python interpreter not found.");
        vscode.window.showErrorMessage(
            `Python interpreter not found in path: ${workspaceSettings.interpreter}`,
        );
        return;
    }

    let command: string;
    if (process.env.DEV_MODE === "true") {
        command = DEV_SITH_BIN_BUILD_PATH;
        logger.info(`Using the SithLSP dev binary build: ${DEV_SITH_BIN_BUILD_PATH}`);
    } else {
        command = await findSithBinaryPath(workspaceSettings);
    }

    const run: Executable = {
        command,
        options: {
            env: {
                ...process.env,
                RUST_LOG: "debug",
            },
        },
    };
    const serverOptions: ServerOptions = {
        run,
        debug: run,
    };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: "file", language: "python" },
            { scheme: "untitled", language: "python" },
        ],
        outputChannel,
        traceOutputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Never,
        initializationOptions: {
            settings: extensionSettings,
            globalSettings,
        },
    };

    // Create the language client and start the client.
    client = new LanguageClient("sith-language-server", "SithLSP", serverOptions, clientOptions);
    client.start();
}

async function stopServer() {
    logger.info("Server stopped");
    await client.stop();
    dispose();
}

function dispose(): void {
  _disposables.forEach((d) => d.dispose());
  _disposables = [];
}