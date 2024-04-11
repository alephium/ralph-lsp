local function ralph_init()

  local capabilities = vim.lsp.protocol.make_client_capabilities()

  capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true

  local root_dir = vim.fs.dirname(vim.fs.find({'build.ralph', 'contracts', 'artifacts'}, { upward = true })[1])

  if root_dir == nil then root_dir = vim.fn.getcwd() end

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

