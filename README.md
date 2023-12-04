# ralph-lsp

Language server for Ralph.

Currently supports text document level events and diagnostics.

# Build the jar

```shell
sbt "project lsp-server; assembly;"
```

Look in `target` folder: `.../ralph-lsp/lsp-server/target/scala-2.13/ralph-lsp.jar`

# Configuration

Create a mandatory config file named `ralph.json` in your project's root directory. You can use the following sample as reference:

`ralph.json`

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

# Run LSP in VSCode

Update the jar
location [here](plugin-vscode/src/extension.ts).

Run the IDE:

```shell
cd plugin-vscode
code .
```

![img.png](docs/img_2.png)

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
     cmd = {'java', '-jar', '<path-to-your-jar>/ralph-lsp.jar'},
     root_dir = root_dir,
     capabilities = capabilities
   })

end

vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'ralph' },
    callback = function() ralph_init() end
})
```
