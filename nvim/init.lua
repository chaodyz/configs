if vim.g.vscode then
  require "codium"
  require "user.comment"
else
  -- ordinary neovim
  require "user.options"
  require "user.keymaps"
  require "user.plugins"
  require "user.colorscheme"
end
