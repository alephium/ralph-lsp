# ralph-lsp

Language server for Ralph.

Currently supports text document level events and diagnostics.

# Install VSCode plugin

Follow these steps:

1. Download: Get the `.vsix` plugin file from the [latest release](https://github.com/alephium/ralph-lsp/releases).
2. Install:
    - Open Visual Studio Code.
    - Go to Extensions > Views and More Actions > Install from VSIX...
    - Select the downloaded `.vsix` file and click "Install".

# Build the JAR

```shell
sbt "compile; lsp-server/assembly"
```

Look in `target` folder: `.../ralph-lsp/lsp-server/target/scala-2.13/ralph-lsp.jar`

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

# Run LSP in neovim

Install the [ralph.vim](https://github.com/tdroxler/ralph.vim) plugin with your favorite plugin manager, for file type detection, highlighting, etc.

## [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)

Add the following to your lua configuration

```lua
local function ralph_init()

  local capabilities = vim.lsp.protocol.make_client_capabilities()

  capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true

  local root_dir = vim.fs.dirname(vim.fs.find({'ralph.json', 'contracts', 'artifacts'}, { upward = true })[1])
  if root_dir == nil then root_dir = vim.fn.getcwd() end

   vim.lsp.start({
     name = 'ralph-lsp',
     cmd = {'java', '-jar', '-DRALPH_LSP_LOG_HOME=<path-to-your-log-folder>', '<path-to-your-jar>/ralph-lsp.jar'},
     root_dir = root_dir,
     capabilities = capabilities
   })

end

vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'ralph' },
    callback = function() ralph_init() end
})
```

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
  "artifactPath": "artifacts",
  "dependencyPath": "dependencies"
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
