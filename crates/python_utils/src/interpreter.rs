use std::{
    env, fs,
    path::{Path, PathBuf},
};

use base64::{engine::general_purpose::URL_SAFE, Engine as _};
use regex::Regex;
use rustc_hash::FxHashSet;
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

use crate::parse_python_version;

/// Attempts to find a Python interpreter by searching through various common locations and
/// configuration files, in a prioritized order.
///
/// This function performs a series of checks to locate a Python interpreter, trying various
/// common patterns:
///
/// 1. Checks the `VIRTUAL_ENV` environment variable for an active virtual environment
/// 2. Checks for the `.venv` directory by searching for the `pyvenv.cfg` file in the project root
/// 3. Checks for Poetry configuration via `pyproject.toml`
/// 4. Checks for Pipenv configuration via `Pipfile`
/// 5. Looks for pyenv configuration via `.python-version` file
/// 6. Checks the `CONDA_PREFIX` environment variable for a possibly active virtual environment
/// 7. As a last resort, performs a system-wide search for Python executables
pub fn resolve_python_interpreter(root: impl AsRef<Path>) -> Option<PathBuf> {
    let mut python_path = None;
    // 1) Try to get the python path from activated VIRTUAL_ENV.
    if let Some(venv_path) = env::var_os("VIRTUAL_ENV") {
        tracing::info!("Found Python path in VIRTUAL_ENV ({venv_path:?})");
        python_path = get_python_path_in_venv(venv_path);
    }

    // 2) Try to find the `.venv` directory by searching for the `pyvenv.cfg` file
    // contained inside the `.venv` dir in the project root.
    if python_path.is_none() {
        if let Some(pyvenv_path) = WalkDir::new(&root)
            .max_depth(2)
            .into_iter()
            .filter_map(|e| e.ok())
            .find(|e| e.path().ends_with("pyvenv.cfg"))
        {
            let venv_path = pyvenv_path.path().parent().unwrap();
            python_path = get_python_path_in_venv(venv_path);

            if python_path.is_some() {
                tracing::info!(
                    "Found Python path in virtual env folder ({})",
                    venv_path.display()
                );
            }
        }
    }

    let project_root = root.as_ref();
    // 3) Try to find the `pyproject.toml` file created by Poetry.
    if project_root.join("pyproject.toml").is_file() && python_path.is_none() {
        python_path = try_to_resolve_poetry_python_venv(project_root);
    }

    // 4) Try to find the `Pipfile` file created by Pipenv.
    if project_root.join("Pipfile").is_file() && python_path.is_none() {
        return try_to_resolve_pipfile_python_venv(project_root);
    }

    // 5) Try to find the `.python-version` file created by the `pyenv local` command.
    if project_root.join(".python-version").is_file() && python_path.is_none() {
        if let Some(pyenv_root) = try_to_find_pyenv_dir() {
            let contents = std::fs::read_to_string(project_root.join(".python-version")).unwrap();
            if !contents.is_empty() {
                let python_version = contents.lines().next().unwrap();
                if parse_python_version(python_version).is_err() {
                    return None;
                }
                let venv_path = pyenv_root.join("versions").join(python_version);

                tracing::info!(
                    "Found Python virtual env directory created by Pyenv at {}",
                    venv_path.display()
                );

                return Some(venv_path.join("bin/python"));
            }
        }
    }

    // 6) Try to get the python path from CONDA_PREFIX.
    if let Some(conda_prefix) = env::var_os("CONDA_PREFIX") {
        tracing::info!("Found Python path in CONDA_PREFIX ({conda_prefix:?})");
        python_path = get_python_path_in_venv(conda_prefix);
    }

    // 7) Try to find a python interpreter doing a system-wide search
    if python_path.is_none() {
        let root_path = get_system_root_path(root.as_ref());
        python_path = try_to_resolve_python_path(root_path);
    }

    python_path
}

fn try_to_find_pyenv_dir() -> Option<PathBuf> {
    if let Some(pyenv_root) = env::var_os("PYENV_ROOT") {
        Some(PathBuf::from(pyenv_root))
    } else {
        // Function to check if an entry is a pyenv directory
        let is_pyenv_dir = |entry: &walkdir::DirEntry| -> bool {
            if !entry.file_type().is_dir() {
                return false;
            }

            let file_name = entry.file_name().to_string_lossy();
            file_name == ".pyenv" || file_name == "pyenv"
        };
        let home_path = dirs::home_dir()?;
        WalkDir::new(home_path)
            .max_depth(5)
            .into_iter()
            .filter_map(Result::ok)
            .find(is_pyenv_dir)
            .map(|e| e.path().to_path_buf())
    }
}

/// Do a system-wide search for the Python executable.
///
/// First, it checks in some common installation paths of Python. If the search
/// in those paths fails, it will recursivly search from the root directory.
///
/// Returns `None` if no python executable was found.
fn try_to_resolve_python_path(root_path: &Path) -> Option<PathBuf> {
    let default_install_paths: FxHashSet<PathBuf> = if cfg!(windows) {
        FxHashSet::from_iter([
            dirs::data_local_dir()
                .unwrap()
                .join("Programs")
                .join("Python"),
            root_path.join("Program Files"),
            root_path.join("Program Files (x86)"),
        ])
    } else {
        FxHashSet::from_iter([
            PathBuf::from("/usr/local/bin"),
            PathBuf::from("/usr/bin"),
            PathBuf::from("/bin"),
            PathBuf::from("/usr/sbin"),
            PathBuf::from("/sbin"),
        ])
    };

    let is_python_executable = |e: &walkdir::DirEntry| {
        let file_name = e.file_name().to_string_lossy();
        (file_name == "python" || file_name == "python.exe") && is_executable(e.path())
    };

    let mut result = None;
    for default_install_path in &default_install_paths {
        result = WalkDir::new(default_install_path)
            .into_iter()
            .filter_entry(|e| e.path().is_file())
            .filter_map(|e| e.ok())
            .find(is_python_executable);

        if result.is_some() {
            break;
        }
    }

    let python_path = if result.is_some() {
        result
    } else {
        // search from the root directory for the python executable
        WalkDir::new(root_path)
            .into_iter()
            .filter_entry(|e| !default_install_paths.contains(e.path()) && e.path().is_file())
            .filter_map(|e| e.ok())
            .find(is_python_executable)
    };

    python_path.and_then(|e| {
        tracing::info!(
            "Found Python interpreter at {}",
            e.path().parent().unwrap().display()
        );
        // resolves any symlinks if any
        fs::canonicalize(e.path()).ok()
    })
}

fn try_to_resolve_poetry_python_venv(project_root: &Path) -> Option<PathBuf> {
    let poetry_config_path = dirs::config_dir()
        .map(|config_path| config_path.join("pypoetry"))
        .or_else(|| env::var_os("POETRY_CONFIG_DIR").map(PathBuf::from))?;

    let local_config = project_root.join("poetry.toml");
    let config_file = if local_config.is_file() {
        tracing::info!("Using Poetry project local config file");
        local_config
    } else {
        tracing::info!("Using global Poetry config file");
        poetry_config_path.join("config.toml")
    };
    let content = fs::read_to_string(config_file).ok()?;
    let parsed_toml: toml::Value = toml::from_str(&content).ok()?;
    let virtual_envs_path_str = parsed_toml
        .get("virtualenvs")
        .and_then(|v| v.get("path"))
        .and_then(|v| v.as_str());
    let cache_dir = parsed_toml.get("cache-dir").and_then(|v| v.as_str());

    let venvs_path = match (virtual_envs_path_str, cache_dir) {
        (Some(venvs_path_str), Some(cache_dir)) => {
            PathBuf::from(venvs_path_str.replace("{cache-dir}", cache_dir))
        }
        // Use the default value for cache-dir
        (Some(venvs_path_str), None) => {
            let cache_dir = dirs::cache_dir()?;
            PathBuf::from(venvs_path_str.replace("{cache-dir}", cache_dir.to_str()?))
        }
        // Use the default value for virtualenvs.path
        (None, Some(cache_dir)) => PathBuf::from(cache_dir).join("virtualenvs"),
        // Poetry config is empty, use default values
        (None, None) => dirs::cache_dir()?.join("pypoetry").join("virtualenvs"),
    };

    // Get the current active virtual env of the project. Poetry stores the python
    // version of the current active virtual env of a project in the `{cache-dir}/virtualenvs/envs.toml` file.
    let envs_file = venvs_path.join("envs.toml");
    let envs_file_content = fs::read_to_string(envs_file).ok()?;
    let project_name = project_root.file_name()?.to_str()?;

    let parsed_toml: toml::Value = toml::from_str(&envs_file_content).ok()?;
    let python_version = parsed_toml
        .as_table()
        .and_then(|e| e.iter().find(|(key, _)| key.starts_with(project_name)))
        .and_then(|(_, value)| value.get("minor"))
        .and_then(|e| e.as_str())
        .unwrap_or("");

    // Find the correct virtual env directory of the project. If `python_version` is an empty
    // string, we will only use the `project_name` for the search which is less accurate.
    return WalkDir::new(&venvs_path)
        .max_depth(1)
        .into_iter()
        .filter_entry(|e| e.path().is_dir())
        .filter_map(|e| e.ok())
        .find(|e| {
            let file_name = e.file_name().to_string_lossy();
            file_name.starts_with(project_name) && file_name.ends_with(python_version)
        })
        .and_then(|venv_base_path| {
            tracing::info!(
                "Found Python virtual env directory created by Poetry at {}",
                venv_base_path.path().display()
            );
            get_python_path_in_venv(venv_base_path.path())
        });
}

fn try_to_resolve_pipfile_python_venv(project_root: &Path) -> Option<PathBuf> {
    let venvs_path = if let Some(venvs_path) = env::var_os("WORKON_HOME") {
        PathBuf::from(venvs_path.to_str()?)
    } else if cfg!(windows) {
        dirs::home_dir()?.join(".virtualenvs")
    } else {
        dirs::data_dir()?.join(".virtualenvs")
    };
    // Implements the project name hashing from Pipenv
    // https://github.com/pypa/pipenv/blob/46bc4ef3b482f1a4fbffee14056780c9c9a8133a/pipenv/project.py#L528-L540
    let sanitize = |name: &str| {
        let re = Regex::new("[&$`!*@\"()\\[\\]\\\r\n\t]").unwrap();
        let replaced = re.replace_all(name, "_");
        replaced.chars().take(42).collect::<String>()
    };
    let project_name = sanitize(project_root.file_name()?.to_str()?);
    let mut hasher = Sha256::new();
    hasher.update(&project_name);
    let hash: Vec<u8> = hasher.finalize().into_iter().take(6).collect();
    let encoded_hash = &URL_SAFE.encode(hash)[..8];
    let venv_name = format!("{project_name}-{encoded_hash}");

    let venv_path = WalkDir::new(venvs_path)
        .max_depth(1)
        .into_iter()
        .filter_entry(|e| e.path().is_dir())
        .filter_map(|e| e.ok())
        .find(|e| {
            e.path()
                .file_name()
                .and_then(|dir_name| dir_name.to_str())
                .is_some_and(|dir_name| dir_name.starts_with(&venv_name))
        })
        .map(|e| e.path().to_path_buf())?;
    tracing::info!(
        "Found Python virtual env directory created by Pipenv at {}",
        venv_path.display()
    );

    get_python_path_in_venv(venv_path)
}

fn get_system_root_path(mut base_path: &Path) -> &Path {
    while let Some(parent) = base_path.parent() {
        base_path = parent;
    }

    base_path
}

/// Given a path to a `.venv` directory returns the path to the python executable.
///
/// In Linux the path to the python executable is a symlink, therefore, we need
/// to check if the symlink is valid, if the symlink isn't valid return `None`.
fn get_python_path_in_venv(venv_path: impl AsRef<Path>) -> Option<PathBuf> {
    if cfg!(windows) {
        Some(venv_path.as_ref().join("Scripts").join("python.exe"))
    } else {
        let python_path = venv_path.as_ref().join("bin").join("python");
        fs::canonicalize(python_path).ok()
    }
}

/// Checks if a file is executable
fn is_executable(path: &Path) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if let Ok(metadata) = path.metadata() {
            return metadata.permissions().mode() & 0o111 != 0;
        }
    }

    #[cfg(windows)]
    {
        // On Windows, we just check if the file exists with an executable extension
        if let Some(ext) = path.extension() {
            return ext == "exe";
        }
    }

    false
}
