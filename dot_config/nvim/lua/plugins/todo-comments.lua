return {
  {
    "folke/todo-comments.nvim",
    enable = true,
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("todo-comments").setup({
        signs = true,
      })
    end
  }
}
