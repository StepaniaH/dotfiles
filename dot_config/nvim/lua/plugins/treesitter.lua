return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require 'nvim-treesitter.configs'.setup {
        ensure_installed = { "c", "lua", "python", "vim", "vimdoc", "query", "markdown", "markdown_inline", 'bash' },
        auto_install = false,
        sync_install = false,
        ignore_install = {},
        modules = {},
        highlight = {
          enable = true,
          disable = function(lang, buf)
            local max_filesize = 100 * 1024                                         -- 100 KB
            local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf)) --My be in new version, the vim.uv should be vim.loop?
            if ok and stats and stats.size > max_filesize then
              return true
            end
            if lang == "python" then --This part do nothing, just for ignoring the parameter alarm.
              return false
            end
          end,
          additional_vim_regex_highlighting = false,
        },

      }
    end,
  }
}
