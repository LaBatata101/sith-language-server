import { workspace, WorkspaceFolder, Uri } from "vscode";
import * as fs from "fs-extra";
import * as path from "path";
import * as vscode from "vscode";
import { logger } from "./logger";
import { FIND_BINARY_SCRIPT_PATH, RUFF_BINARY_NAME, SITH_BINARY_NAME } from "./constants";
import which = require("which");
import { platform } from "os";
import { execFile } from "child_process";
import { LogOutputChannel } from "vscode";
import { ISettings } from "./settings";

export function getWorkspaceFolders(): readonly WorkspaceFolder[] {
    return workspace.workspaceFolders ?? [];
}

export async function getProjectRoot(): Promise<WorkspaceFolder> {
    const workspaces: readonly WorkspaceFolder[] = getWorkspaceFolders();
    if (workspaces.length === 0) {
        return {
            uri: Uri.file(process.cwd()),
            name: path.basename(process.cwd()),
            index: 0,
        };
    } else if (workspaces.length === 1) {
        return workspaces[0];
    } else {
        let rootWorkspace = workspaces[0];
        let root = undefined;
        for (const w of workspaces) {
            if (await fs.pathExists(w.uri.fsPath)) {
                root = w.uri.fsPath;
                rootWorkspace = w;
                break;
            }
        }

        for (const w of workspaces) {
            if (root && root.length > w.uri.fsPath.length && (await fs.pathExists(w.uri.fsPath))) {
                root = w.uri.fsPath;
                rootWorkspace = w;
            }
        }
        return rootWorkspace;
    }
}

/**
 * Finds the Ruff binary path and returns it.
 *
 * The strategy is as follows:
 * 1. If the 'path' setting is set, check each path in order. The first valid
 *    path is returned.
 * 2. Execute a Python script that tries to locate the binary. This uses either
 *    the user-provided interpreter or the interpreter provided by the Python
 *    extension.
 * 3. If the Python script doesn't return a path, check the global environment
 *    which checks the PATH environment variable.
 * 4. If all else fails, return null.
 */
export async function findRuffBinaryPath(
    settings: ISettings,
    outputChannel: LogOutputChannel,
): Promise<string> {
    // 'path' setting takes priority over everything.
    if (settings.ruff.path) {
        if (await fs.pathExists(settings.ruff.path)) {
            logger.info(`Using 'ruff.path' setting: ${settings.ruff.path}`);
            return settings.ruff.path;
        }
        logger.info(`Could not find Ruff executable in 'path': ${settings.ruff.path}`);
    }

    // Otherwise, we'll call a Python script that tries to locate a binary.
    let ruffBinaryPath: string | undefined;
    try {
        const stdout = await executeFile(settings.interpreter, [
            FIND_BINARY_SCRIPT_PATH,
            RUFF_BINARY_NAME,
        ]);
        ruffBinaryPath = stdout.trim();
    } catch (err) {
        vscode.window
            .showErrorMessage(
                "Unexpected error while trying to find the Ruff binary. See the logs for more details.",
                "Show Logs",
            )
            .then((selection) => {
                if (selection) {
                    outputChannel.show();
                }
            });
        logger.error(`Error while trying to find the Ruff binary: ${err}`);
    }

    if (ruffBinaryPath && ruffBinaryPath.length > 0) {
        // First choice: the executable found by the script.
        logger.info(`Using the Ruff binary: ${ruffBinaryPath}`);
        return ruffBinaryPath;
    }

    // Second choice: the executable in the global environment.
    const environmentPath = await which(RUFF_BINARY_NAME, { nothrow: true });
    if (environmentPath) {
        logger.info(`Using environment executable: ${environmentPath}`);
        return environmentPath;
    }

    return null;
}

/**
 * Finds the Sith binary path and returns it.
 *
 * The strategy is as follows:
 * 1. If the 'path' setting is set, check each path in order. The first valid
 *    path is returned.
 * 2. Execute a Python script that tries to locate the binary. This uses either
 *    the user-provided interpreter or the interpreter provided by the Python
 *    extension.
 * 3. If the Python script doesn't return a path, check the global environment
 *    which checks the PATH environment variable.
 * 4. If all else fails, return null.
 */
export async function findSithBinaryPath(
    settings: ISettings,
): Promise<string> {
    if (settings.executable) {
        if (await fs.pathExists(settings.executable)) {
            logger.info(`Using 'executable' setting: ${settings.executable}`);
            return settings.executable;
        }
        logger.info(`Could not find SithLSP executable in 'path': ${settings.executable}`);
    }

    // Otherwise, we'll call a Python script that tries to locate a binary.
    let sithBinaryPath: string | undefined;
    try {
        const stdout = await executeFile(settings.interpreter, [
            FIND_BINARY_SCRIPT_PATH,
            SITH_BINARY_NAME,
        ]);
        sithBinaryPath = stdout.trim();
    } catch (err) {
        vscode.window
            .showErrorMessage(
                "Unexpected error while trying to find the SithLSP binary. See the logs for more details.",
                "Show Logs",
            )
            .then((selection) => {
                if (selection) {
                    logger.channel.show();
                }
            });
        logger.error(`Error while trying to find the SithLSP binary: ${err}`);
    }

    if (sithBinaryPath && sithBinaryPath.length > 0) {
        // First choice: the executable found by the script.
        logger.info(`Using the SithLSP binary: ${sithBinaryPath}`);
        return sithBinaryPath;
    }

    // Second choice: the executable in the global environment.
    const environmentPath = await which(SITH_BINARY_NAME, { nothrow: true });
    if (environmentPath) {
        logger.info(`Using environment executable: ${environmentPath}`);
        return environmentPath;
    }

    return null;
}

/**
 * Check if shell mode is required for `execFile`.
 *
 * The conditions are:
 * - Windows OS
 * - File extension is `.cmd` or `.bat`
 */
export function execFileShellModeRequired(file: string) {
    file = file.toLowerCase();
    return platform() === "win32" && (file.endsWith(".cmd") || file.endsWith(".bat"));
}

/**
 * Function to execute a command and return the stdout.
 */
function executeFile(file: string, args: string[] = []): Promise<string> {
    const shell = execFileShellModeRequired(file);
    return new Promise((resolve, reject) => {
        execFile(shell ? `"${file}"` : file, args, { shell }, (error, stdout, stderr) => {
            if (error) {
                reject(new Error(stderr || error.message));
            } else {
                resolve(stdout);
            }
        });
    });
}
