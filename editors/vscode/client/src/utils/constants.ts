import * as path from "path";

const folderName = path.basename(__dirname);

/**
 * Name of the `ruff` binary based on the current platform.
 */
export const RUFF_BINARY_NAME = process.platform === "win32" ? "ruff.exe" : "ruff";

/**
 * Name of the `ruff` binary based on the current platform.
 */
export const SITH_BINARY_NAME = process.platform === "win32" ? "sith-lsp.exe" : "sith-lsp";

/**
 * Path to the root directory of this extension.
 */
export const EXTENSION_ROOT_DIR =
    folderName === "utils" ? path.dirname(path.dirname(__dirname)) : path.dirname(__dirname);

/**
 * Path to the directory containing the bundled Python scripts.
 */
export const BUNDLED_PYTHON_SCRIPTS_DIR = path.join(EXTENSION_ROOT_DIR, "bundled");

/**
 * Path to the Python script that tries to find the Ruff binary path.
 *
 * This should only be used as a fallback if there is no valid `ruff` binary in
 * the user's `path` setting or the import strategy isn't `useBundled`.
 */
export const FIND_BINARY_SCRIPT_PATH = path.join(
    BUNDLED_PYTHON_SCRIPTS_DIR,
    "tool",
    "find_binary_path.py",
);

const SITH_PROJECT_DIR =
    folderName === "utils"
        ? path.dirname(path.dirname(path.dirname(path.dirname(__dirname))))
        : path.dirname(path.dirname(path.dirname(__dirname)));

export const DEV_SITH_BIN_BUILD_PATH = path.join(
    SITH_PROJECT_DIR,
    "target",
    "debug",
    SITH_BINARY_NAME,
);
