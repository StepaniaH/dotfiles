local opt = vim.opt
local g = vim.g

g.mapleader = " "
g.maplocalleader = "\\"
g.autoformat = false
g.have_nerd_font = true

opt.number = true
opt.relativenumber = true
opt.wrap = true
opt.expandtab = true
opt.tabstop = 2
opt.softtabstop = 2
opt.shiftwidth = 2
opt.clipboard = "unnamedplus"
opt.mouse = 'a'
opt.spell = false
opt.ignorecase = true
opt.smartcase = true
opt.signcolumn = 'yes'
opt.splitright = true
opt.splitbelow = true
opt.list = true -- See `help 'list'`
opt.listchars = {
  space = "·",
  tab = "▸ ",
  nbsp = "␣",
  trail = "·",
  extends = "⟩",
  precedes = "⟨",
}
opt.inccommand = "split"
opt.cursorline = true
opt.scrolloff = 10
opt.breakindent = true
opt.termguicolors = true

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end
})
