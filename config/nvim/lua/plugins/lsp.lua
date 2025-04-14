return {
  {
    "neovim/nvim-lspconfig",
    enable = false,
    dependencies = {
      { "saghen/blink.cmp" },
    },
    config = function()
      local capabilities = require('blink.cmp').get_lsp_capabilities()
      -- Lua language serve
      require('lspconfig').lua_ls.setup({
        settings = {
          Lua = {
            diagnostics = {
              globals = { 'vim' }
            }
          }
        },
        capabilities = capabilities,
      })
      -- C language serve
      require('lspconfig').clangd.setup { capabilities = capabilities }
      -- Python language serve
      require('lspconfig').pyright.setup { capabilities = capabilities }

      -- Auto formatting when save
      vim.api.nvim_create_autocmd('LspAttach', {
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if not client then return end
          if client.supports_method('textDocument/formatting') then
            vim.api.nvim_create_autocmd('BufWritePre', {
              buffer = args.buf,
              callback = function()
                vim.lsp.buf.format({ bufnr = args.buf, id = client.id })
              end,
            })
          end
        end,
      })
    end,
  }
}
