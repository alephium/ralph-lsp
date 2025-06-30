# Ralph Nvim pluggin

Nvim plugin for [Alephium](https://alephium.org/) smart contract programming language: [Ralph](https://wiki.alephium.org/ralph/getting-started/)

This plugin use [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig) to provide LSP client

## Features

* [Publish Diagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics)
* [Goto Definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition)
* [Find References](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references)
* [Completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion)
* [Rename](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename)
* [Hover](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover)
* [Workspace Folders](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_workspaceFolders)
* [Inlay Hints](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint)
* More to come...

## Workspace folders

Ralph.nvim supports multiple workspace projects. You can use the [Workspace Folders Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_workspaceFolders) to manage multiple ralph projects within the same Neovim session.

To add a workspace folder, run the following command in Neovim:

```vim
:lua vim.lsp.buf.add_workspace_folder()
```

This allows you to register additional project folders, and `ralph-lsp` will handle each project separately.
