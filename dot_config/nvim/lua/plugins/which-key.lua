return {
  {
    "folke/which-key.nvim",
    enable = true,
    event = "VeryLazy",
    config = function()
      require("which-key").setup({})

      vim.keymap.set("n", "<leader>?", function()
        require("which-key").show({ global = false })
      end, { desc = "Buffer Local Keymaps (which-key)" })
    end
  }
}
