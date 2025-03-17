local function ralph_init()

  local capabilities = vim.lsp.protocol.make_client_capabilities()

  capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true

  local function find_project_root(fname)
    return vim.fs.dirname(vim.fs.find({'alephium.config.ts', '.ralph-lsp', 'contracts', 'artifacts', '.git'}, { upward = true, path = fname })[1])
      or vim.fn.getcwd()
  end

  local buf = vim.api.nvim_get_current_buf()
  local root_dir = find_project_root(vim.api.nvim_buf_get_name(buf))

  vim.lsp.start({
    name = 'ralph-lsp',
    cmd = {'ralph-lsp'},
    root_dir = root_dir,
    capabilities = capabilities
  })
end

vim.api.nvim_create_autocmd('FileType', {
    pattern = { 'ralph' },
    callback = function() ralph_init() end
})

