vim.cmd [[
try
  " colorscheme darkplus
  set termguicolors
  set background=dark " or dark
  colorscheme zenburned
catch /^Vim\%((\a\+)\)\=:E185/
  colorscheme default
  set background=dark
endtry
]]
