# ralph-lsp

Language server for Ralph.

# Features

- [Publish Diagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics)
- [Goto Definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition)
- [Find References](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references)
- [Completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion)
- [Rename](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename)
- More to come...

# Requirements

Java 11 or higher.

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

Configurations in `alephium.config.ts` are the primary source for build settings, and `ralph.json` is secondary.
Any settings defined in `alephium.config.ts` will be mirrored in `ralph.json`.

## `alephium.config.ts`

Refer to the [documentation](https://docs.alephium.org/sdk/cli/#configuration) for more details about the `alephium.config.ts`.

## `ralph.json`

Once your IDE has booted up, a config file named `ralph.json` will be generated in your project's root
directory under the folder `.ralph-lsp/ralph.json`. 
This file reflects the `sourceDir` and `compilerOptions` settings defined in `alephium.config.ts`.
If `alephium.config.ts` is not present or if the settings are missing, default settings will be used.

The `dependencyPath` setting is specific to Ralph-LSP and configures the directory for dependencies.
If `dependencyPath` is not specified, the default path (`<user.home>/.ralph-lsp/dependencies/`) will be used.

Here’s an example of the `ralph.json` file with all configurations set:

```json
{
  "contractPath": "contracts",
  "dependencyPath": "dependencies",
  "compilerOptions": {
    "ignoreUnusedConstantsWarnings": false,
    "ignoreUnusedVariablesWarnings": false,
    "ignoreUnusedFieldsWarnings": false,
    "ignoreUnusedPrivateFunctionsWarnings": false,
    "ignoreUpdateFieldsCheckWarnings": false,
    "ignoreCheckExternalCallerWarnings": false,
    "ignoreUnusedFunctionReturnWarnings": false
  }
}
```

## Configure trace (VSCode)

![image](https://github.com/alephium/ralph-lsp/assets/1773953/ac537faf-492f-468a-ab88-a39323e6e821)

- `off` - Enables `info`, `warning` and `error`.
- `messages` - Enables all the above including `debug`.
- `verbose` - Enables all the above including `trace`.

# Release VSCode plugin

- Run "[Build the JAR for VSCode](#build-the-jar-for-vscode)"
- Run `vsce package <version>`
- Run `vsce publish`
