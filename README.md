# ralph-lsp

Language server for Ralph.

Currently supports text document level events and diagnostics.

# Installation

Go to the [latest release](https://github.com/alephium/ralph-lsp/releases/latest)

There is currently two ways to install the LSP server, as an executable or as a jar file.

Please note that the server is meant to be run from a client (like an IDE) and not directly.

1. Executable

    Download `ralph-lsp.zip`, extract it and add `ralph-lsp/bin` to your `PATH` environment variable.
    You can run the server with `ralph-lsp` command.

2. Jar

    Download `ralph-lsp.jar` and run it with `java -jar ralph-lsp.jar`

# Install VSCode plugin

Follow these steps:

1. Download: Get the `.vsix` plugin file from the [latest release](https://github.com/alephium/ralph-lsp/releases/latest).
2. Install:
    - Open Visual Studio Code.
    - Go to Extensions > Views and More Actions > Install from VSIX...
    - Select the downloaded `.vsix` file and click "Install".

# Install Neovim plugin

Install [plugin-nvim](/plugin-nvim) with your favorite plugin manager, for example with [vim-plug](https://github.com/junegunn/vim-plug)
```vim
  Plug 'alephium/ralph-lsp', {'rtp': 'plugin-nvim'}
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

After your IDE has booted up, create a mandatory config file named `ralph.json` in your project's root directory.
You can use the following sample as reference:

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
used.

```json
"dependencyPath": "dependencies"
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
