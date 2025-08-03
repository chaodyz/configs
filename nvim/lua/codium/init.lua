--[[
=============================================================
 Import .vim settings, use Packer for plugins.
=============================================================
]]

-- load keymaps
vim.cmd([[source $HOME/.config/nvim/lua/codium/settings.vim]])

-- Always use the clipboard for All Operations
vim.opt.clipboard:append("unnamedplus");

--[[
=============================================================
 Packer for plugins
=============================================================
]]
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

-- configure plugins
require('packer').startup(function(use)
    -- use packer itself
    use 'wbthomason/packer.nvim'
    -- comment/uncomment
    use  'numToStr/Comment.nvim'
    use "JoosepAlviste/nvim-ts-context-commentstring"
    -- surround text objects
    use 'machakann/vim-sandwich'

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if PACKER_BOOTSTRAP then
        require('packer').sync()
    end
end
)
