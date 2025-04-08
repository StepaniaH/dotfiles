return {
  {
    'windwp/nvim-autopairs',
    enabled = true,
    event = "InsertEnter",
    config = function()
      require('nvim-autopairs').setup({
        disable_filetype = { "TelescopePrompt", "spectre_panel" },
        disable_in_macro = true,
        disable_in_visualblock = true,
        disable_in_replace_mode = true,
        ignored_next_char = [=[[%w%%%'%[%"%.%`%$]]=],
        enable_moveright = true,
        enable_afterquote = true,
        enable_check_bracket_line = true,
        enable_bracket_in_quote = true,
        enable_abbr = false,
        break_undo = true,
        check_ts = true,
        map_cr = true,
        map_bs = true,
        map_c_h = false,
        map_c_w = false,
      })
    end
  }
}
