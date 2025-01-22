import { commands } from "vscode";
import { Disposable } from "vscode-languageclient";

export function registerCommand(
    command: string,
    callback: (...args: any[]) => any,
    thisArg?: any,
  ): Disposable {
    return commands.registerCommand(command, callback, thisArg);
  }