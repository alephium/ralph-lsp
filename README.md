# ralph-lsp

Language server for Ralph.

Currently supports text document level events and diagnostics.

# Install VSCode plugin

`Ralph LSP` is available on the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=alephium.ralph-lsp).

### Manual Installation

Follow these steps:

1. Download: Get the `.vsix` plugin file from the [latest release](https://github.com/alephium/ralph-lsp/releases/latest).
2. Install:
    - Open Visual Studio Code.
    - Go to Extensions > Views and More Actions > Install from VSIX...
    - Select the downloaded `.vsix` file and click "Install".

# Install Neovim plugin

Go to the [latest release](https://github.com/alephium/ralph-lsp/releases/latest)

Download `ralph-lsp.zip`, extract it and add `ralph-lsp/bin` to your `PATH` environment variable.

### [vim-plug](https://github.com/junegunn/vim-plug)

```vim
  Plug 'alephium/ralph-lsp', {'rtp': 'plugin-nvim'}
```

### [lazy](https://github.com/folke/lazy.nvim)

```lua
lazy_opts = {
    performance = {
        rtp = {
            paths = {
                vim.fn.stdpath('data') .. '/lazy/ralph-lsp/plugin-nvim'
            }
        },
    },
}

lazy_plugin_config = {
   "alephium/ralph-lsp",
   init = function()
       -- expose directly from inside nvim
       local path = vim.env.PATH
       path = path .. ":" .. "/path/to/ralph-lsp/bin"
       vim.env.PATH = path
   end
}
```

The plugin adds file type detection, syntax highlighting and start the LSP server, make sure you have `ralph-lsp` available in your `PATH`

# Integrate new editor

Please help us integrate Ralph LSP into your favorite editor. We are happy to assist you with any questions you may have.

You can either create your own repository or add a new folder in this repository following the `plugin-<editor>` naming convention.

### Define Root Directory

It's important to ensure that your plugin can find the correct root directory for your Ralph project.
For example, if you open your editor while inside the `contracts` folder, the plugin should be able to identify the root directory as `../contracts`.

In our current plugins, we follow this heuristic:

* Look for the presence of either `alephium.config.ts`, `contracts`, or `.ralph.json`.

If none of these are found, the plugin will use the current directory as the root directory.

# Build the JAR

```shell
sbt "compile; lsp-server/assembly"
```

Look in `target` folder: `.../ralph-lsp/lsp-server/target/scala-2.13/ralph-lsp.jar`

# Build the executable

```shell
sbt universal:packageBin
```
zip file will be generated in `target/universal/ralph-lsp.zip`

For local development, you can run
```shell
sbt stage
```

This creates the `target/universal/stage/bin/ralph-lsp` executable

# Build the JAR for VSCode

```shell
sbt "compile; lsp-server/assembly; copyJARToVSCode"
```

The JAR file gets generated to `plugin-vscode/out/ralph-lsp.jar`.

# Run LSP in VSCode

Open the plugin directory `plugin-vscode` in VSCode:

```shell
cd plugin-vscode
code .
```

Run the plugin by selecting the menu option `Run -> Run Without Debugging` or `Run -> Start Debugging`.

# Configuration

After your IDE has booted up, a config file named `ralph.json` is generated in your project's root
directory under the folder `.ralph-lsp/ralph.json`. The file contains the following default values:

```json
{
  "compilerOptions": {
    "ignoreUnusedConstantsWarnings": false,
    "ignoreUnusedVariablesWarnings": false,
    "ignoreUnusedFieldsWarnings": false,
    "ignoreUnusedPrivateFunctionsWarnings": false,
    "ignoreUpdateFieldsCheckWarnings": false,
    "ignoreCheckExternalCallerWarnings": false
  },
  "contractPath": "contracts",
  "artifactPath": "artifacts"
}
```

The `dependencyPath` field is optional. If not set, the default path (`<user.home>/.ralph-lsp/dependencies/`) will be
used. If you wish to specify a custom path, add the following to your `ralph.json` file:

```json
"dependencyPath": "dependencies"
```

This configuration allows you to customise the behavior of the compiler and define the paths for
your contracts, artifacts, and dependencies.

## Configure trace (VSCode)

![image](https://github.com/alephium/ralph-lsp/assets/1773953/ac537faf-492f-468a-ab88-a39323e6e821)

- `off` - Enables `info`, `warning` and `error`.
- `messages` - Enables all the above including `debug`.
- `verbose` - Enables all the above including `trace`.

# Release VSCode plugin

- Run "[Build the JAR for VSCode](#build-the-jar-for-vscode)"
- Run `vsce package <version>`
- Run `vsce publish`
