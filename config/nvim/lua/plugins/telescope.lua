return {
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = {
      { 'nvim-lua/plenary.nvim' },
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    },
    config = function()
      require('telescope').setup {
        pickers = {
          find_files = {
            theme = "ivy",
            hidden = true,
            follow = true,
          }
        },
        extensions = {
          fzf = {}
        }
      }
      require('telescope').load_extension('fzf')

      -- Find help tags
      vim.keymap.set("n", "<leader>fh", require('telescope.builtin').help_tags)
      -- Find files at current location
      vim.keymap.set("n", "<leader>fd", require('telescope.builtin').find_files)
      -- Find files in configuration directory
      vim.keymap.set("n", "<leader>en", function()
        require('telescope.builtin').find_files {
          cwd = vim.fn.stdpath("config")
        }
      end)
      -- Find files in plugins' directory
      vim.keymap.set("n", "<leader>ep", function()
        require('telescope.builtin').find_files {
          cwd = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy")
        }
      end)
    end
  }
}
