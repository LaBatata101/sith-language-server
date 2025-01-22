import {
    workspace,
    ConfigurationChangeEvent,
    WorkspaceFolder,
    WorkspaceConfiguration,
    ConfigurationScope,
} from "vscode";
import { getWorkspaceFolders } from "./fs";

export type LogLevel = "error" | "warn" | "info" | "debug" | "trace";

export type Lint = {
    enable: boolean;
    select?: string[];
    extendSelect?: string[];
    ignore?: string[];
};

export type Format = {
    enable: boolean;
    args?: string[];
};

export type Ruff = {
    path: string;
    format: Format;
    lint: Lint;
};

export interface ISettings {
    workspace: string;
    executable: string;
    interpreter: string;
    ruff: Ruff;
    logLevel?: LogLevel;
    logFile?: string;
}

export function getExtensionSettings(namespace: string): ISettings[] {
    return getWorkspaceFolders().map((workspaceFolder) =>
        getWorkspaceSettings(namespace, workspaceFolder),
    );
}

export function getConfiguration(
    config: string,
    scope?: ConfigurationScope,
): WorkspaceConfiguration {
    return workspace.getConfiguration(config, scope);
}

function getInterpreterFromSetting(namespace: string, scope?: ConfigurationScope) {
    const config = getConfiguration(namespace, scope);
    return config.get<string>("interpreter");
}

export function getWorkspaceSettings(namespace: string, workspace: WorkspaceFolder): ISettings {
    const config = getConfiguration(namespace);
    let interpreter: string = getInterpreterFromSetting(namespace, workspace) ?? "";

    return {
        workspace: workspace.uri.fsPath,
        executable: config.get<string>("executable") ?? "sith-lsp",
        interpreter,
        ruff: {
            path: config.get<string>("ruff.path"),
            format: {
                enable: config.get<boolean>("ruff.format.enable") ?? true,
                args: config.get<string[]>("ruff.format.args") ?? [],
            },
            lint: {
                enable: config.get<boolean>("ruff.lint.enable") ?? true,
                select: config.get<string[]>("ruff.lint.select"),
                extendSelect: config.get<string[]>("ruff.lint.extendSelect"),
                ignore: config.get<string[]>("ruff.lint.ignore")
            }
        },
    };
}

function getGlobalValue<T>(config: WorkspaceConfiguration, key: string, defaultValue: T): T {
    const inspect = config.inspect<T>(key);
    return inspect?.globalValue ?? inspect?.defaultValue ?? defaultValue;
}

function getOptionalGlobalValue<T>(config: WorkspaceConfiguration, key: string): T | undefined {
    const inspect = config.inspect<T>(key);
    return inspect?.globalValue;
}

export function getGlobalSettings(namespace: string): ISettings {
    const config = getConfiguration(namespace);
    return {
        workspace: process.cwd(),
        executable: getGlobalValue<string>(config, "executable", "sith-lsp"),
        interpreter: getGlobalValue<string>(config, "interpreter", ""),
        ruff: {
            path: config.get<string>("ruff.path"),
            format: {
                enable: getGlobalValue<boolean>(config, "ruff.format.enable", true),
                args: getGlobalValue<string[]>(config, "ruff.format.args", []),
            },
            lint: {
                enable: getGlobalValue<boolean>(config, "ruff.lint.enable", true),
                select: getOptionalGlobalValue<string[]>(config, "ruff.lint.select"),
                extendSelect: getOptionalGlobalValue<string[]>(config, "ruff.lint.extendSelect"),
                ignore: getOptionalGlobalValue<string[]>(config, "ruff.lint.ignore")
            }
        },
        logLevel: config.get<LogLevel>("logLevel"),
        logFile: config.get<string>("logFile"),
    };
}
export function checkIfConfigurationChanged(
    e: ConfigurationChangeEvent,
    namespace: string,
): boolean {
    const settings = [
        `${namespace}.executable`,
        `${namespace}.interpreter`,
        `${namespace}.ruff.path`,
        `${namespace}.ruff.format.args`,
        `${namespace}.ruff.format.enable`,
        `${namespace}.ruff.lint.enable`,
        `${namespace}.ruff.lint.select`,
        `${namespace}.ruff.lint.extendSelect`,
        `${namespace}.ruff.lint.ignore`,
        `${namespace}.logLevel`,
        `${namespace}.logFile`,
    ];
    return settings.some((s) => e.affectsConfiguration(s));
}
