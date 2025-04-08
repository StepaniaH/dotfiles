return {
  { 
    'echasnovski/mini.nvim',
    enabled = true,
    version = false, 
    config = function()
      local statusline = require 'mini.statusline'
      statusline.setup { use_icon = true }
    end
  }
}
