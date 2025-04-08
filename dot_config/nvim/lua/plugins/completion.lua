return {
  {
    'saghen/blink.cmp',
    dependencies = {
      { 'rafamadriz/friendly-snippets' },
      { 'windwp/nvim-autopairs' },
    },
    version = '*',
    config = function()
      require('blink.cmp').setup({
        keymap = { preset = 'default' },
        appearance = {
          use_nvim_cmp_as_default = true,
          nerd_font_variant = 'mono'
        },
        completion = { accept = { auto_brackets = { enabled = true } } }, -- co-op with autopairs?[ISSUE#157]
      })
    end,
    signature = { enable = true }
  }
}
