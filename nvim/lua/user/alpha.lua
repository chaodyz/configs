local status_ok, alpha = pcall(require, "alpha")
if not status_ok then
	return
end

local dashboard = require("alpha.themes.dashboard")
dashboard.section.header.val = {
	[[                                                                              ]],
	[[                                                                              ]],
	[[                                                                              ]],
	"                                                     ",
	"  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ",
	"  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ",
	"  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ",
	"  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ",
	"  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ",
	"  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ",
	[[                                                                              ]],
	[[                                    ██████                                    ]],
	[[                                ████▒▒▒▒▒▒████                                ]],
	[[                              ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                              ]],
	[[                            ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                            ]],
	[[                          ██▒▒▒▒▒▒▒▒    ▒▒▒▒▒▒▒▒                              ]],
	[[                          ██▒▒▒▒▒▒  ▒▒▓▓▒▒▒▒▒▒  ▓▓▓▓                          ]],
	[[                          ██▒▒▒▒▒▒  ▒▒▓▓▒▒▒▒▒▒  ▒▒▓▓                          ]],
	[[                        ██▒▒▒▒▒▒▒▒▒▒    ▒▒▒▒▒▒▒▒    ██                        ]],
	[[                        ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                        ]],
	[[                        ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                        ]],
	[[                        ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                        ]],
	[[                        ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                        ]],
	[[                        ██▒▒██▒▒▒▒▒▒██▒▒▒▒▒▒▒▒██▒▒▒▒██                        ]],
	[[                        ████  ██▒▒██  ██▒▒▒▒██  ██▒▒██                        ]],
	[[                        ██      ██      ████      ████                        ]],
	[[                                                                              ]],
	[[                                                                              ]],
	"                                                                                ",
}
dashboard.section.buttons.val = {
	dashboard.button("f", "  Find file", ":Telescope find_files <CR>"),
	dashboard.button("e", "  New file", ":ene <BAR> startinsert <CR>"),
	dashboard.button("p", "  Find project", ":Telescope projects <CR>"),
	dashboard.button("r", "  Recently used files", ":Telescope oldfiles <CR>"),
	dashboard.button("t", "  Find text", ":Telescope live_grep <CR>"),
	dashboard.button("c", "  Configuration", ":e ~/.config/nvim/init.lua <CR>"),
	dashboard.button("q", "  Quit Neovim", ":qa<CR>"),
}

local function footer()
	-- NOTE: requires the fortune-mod package to work
	-- local handle = io.popen("fortune")
	-- local fortune = handle:read("*a")
	-- handle:close()
	-- return fortune
	return ""
end

dashboard.section.footer.val = footer()

dashboard.section.footer.opts.hl = "Type"
dashboard.section.header.opts.hl = "Include"
dashboard.section.buttons.opts.hl = "Keyword"

dashboard.opts.opts.noautocmd = true
-- vim.cmd([[autocmd User AlphaReady echo 'ready']])
alpha.setup(dashboard.opts)

--
-- -- plugins/alpha -*- highly customizable dashboard
-- -- vim: foldmethod=marker
--
-- local M = {}
--
-- -- Where all the dashboard parts will be stored
-- local parts = {}
--
-- -- Util function
-- local function create_handle(cmd)
--     local handle = io.popen(cmd) -- Use the popen func to create a handle
--     local data = handle:read('*a') -- Read all the data from the hanlde
--     handle:close() -- close the handle (so we can spawn other ones)
--     return data -- return the wanted data
-- end
-- -- Small function to create buttons {{{
-- local function create_button(shortcut, text, cmd)
--     local sc = shortcut:gsub('%s', ''):gsub('LDR', '<space>')
--
--     local button_opts = {
--         position = 'center',
--         text = text,
--         shortcut = shortcut,
--         cursor = 5,
--         width = 30,
--         align_shortcut = 'right',
--         hl_shortcut = 'AlphaBody',
--         hl = 'AlphaBody',
--     }
--     if cmd then
--         button_opts.keymap = { 'n', sc, cmd, { noremap = true, silent = true } }
--     end
--
--     -- Returnd default button data
--     return {
--         type = 'button',
--         val = text,
--         on_press = function()
--             local key = vim.api.nvim_replace_termcodes(sc, true, false, true)
--             vim.api.nvim_feedkeys(key, 'normal', false)
--         end,
--         opts = button_opts,
--     }
-- end
-- -- }}}
--
-- -- Dashboard header (art from glepnir/dashboard-nvim)
-- parts.header = { type = 'text', opts = { position = 'center', hl = 'AlphaHeader' } }
-- parts.header.val = {
--     [[⡆⣐⢕⢕⢕⢕⢕⢕⢕⢕⠅⢗⢕⢕⢕⢕⢕⢕⢕⠕⠕⢕⢕⢕⢕⢕⢕⢕⢕⢕]],
--     [[⢐⢕⢕⢕⢕⢕⣕⢕⢕⠕⠁⢕⢕⢕⢕⢕⢕⢕⢕⠅⡄⢕⢕⢕⢕⢕⢕⢕⢕⢕]],
--     [[⢕⢕⢕⢕⢕⠅⢗⢕⠕⣠⠄⣗⢕⢕⠕⢕⢕⢕⠕⢠⣿⠐⢕⢕⢕⠑⢕⢕⠵⢕]],
--     [[⢕⢕⢕⢕⠁⢜⠕⢁⣴⣿⡇⢓⢕⢵⢐⢕⢕⠕⢁⣾⢿⣧⠑⢕⢕⠄⢑⢕⠅⢕]],
--     [[⢕⢕⠵⢁⠔⢁⣤⣤⣶⣶⣶⡐⣕⢽⠐⢕⠕⣡⣾⣶⣶⣶⣤⡁⢓⢕⠄⢑⢅⢑]],
--     [[⠍⣧⠄⣶⣾⣿⣿⣿⣿⣿⣿⣷⣔⢕⢄⢡⣾⣿⣿⣿⣿⣿⣿⣿⣦⡑⢕⢤⠱⢐]],
--     [[⢠⢕⠅⣾⣿⠋⢿⣿⣿⣿⠉⣿⣿⣷⣦⣶⣽⣿⣿⠈⣿⣿⣿⣿⠏⢹⣷⣷⡅⢐]],
--     [[⣔⢕⢥⢻⣿⡀⠈⠛⠛⠁⢠⣿⣿⣿⣿⣿⣿⣿⣿⡀⠈⠛⠛⠁⠄⣼⣿⣿⡇⢔]],
--     [[⢕⢕⢽⢸⢟⢟⢖⢖⢤⣶⡟⢻⣿⡿⠻⣿⣿⡟⢀⣿⣦⢤⢤⢔⢞⢿⢿⣿⠁⢕]],
--     [[⢕⢕⠅⣐⢕⢕⢕⢕⢕⣿⣿⡄⠛⢀⣦⠈⠛⢁⣼⣿⢗⢕⢕⢕⢕⢕⢕⡏⣘⢕]],
--     [[⢕⢕⠅⢓⣕⣕⣕⣕⣵⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣷⣕⢕⢕⢕⢕⡵⢀⢕⢕]],
--     [[⢑⢕⠃⡈⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢃⢕⢕⢕]],
--     [[⣆⢕⠄⢱⣄⠛⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⢁⢕⢕⠕⢁]],
--     [[⣿⣦⡀⣿⣿⣷⣶⣬⣍⣛⣛⣛⡛⠿⠿⠿⠛⠛⢛⣛⣉⣭⣤⣂⢜⠕⢑⣡⣴⣿]],
-- }
--
-- -- Dashboard current info (between header and buttons)
-- local date = create_handle([[date +"%a %d %b" | tr -d "\n"]])
-- parts.date = { type = 'text', opts = { position = 'center', hl = 'AlphaHeader' } }
-- parts.date.val = '   Today is ' .. date .. ' '
--
-- -- Loaded plugin count
-- local plugin_count = create_handle(
--     [[ls $HOME/.local/share/nvim/site/pack/packer/start/ -l | wc -l | tr -d "\n"]]
-- )
-- parts.plugins = { type = 'text', opts = { position = 'center', hl = 'AlphaHeader' } }
-- parts.plugins.val = '   Loaded ' .. plugin_count .. ' plugins '
--
-- -- Dashboard buttons
-- parts.buttons = { type = 'group', opts = { spacing = 0 } }
-- parts.buttons.val = {
--     create_button('LDR n', ' → New File', ':enew<BAR>startinsert<CR>'),
--     create_button('LDR f f', ' → Find Files', ':Telescope find_files<CR>'),
--     create_button('LDR f o', 'ﭯ → Recent Files', ':Telescope oldfiles<CR>'),
--     create_button('LDR /', ' → Ripgrep', ':Telescope live_grep<CR>'),
--     create_button('LDR f p', ' → Projects', ':Telescope project<CR>'),
--     create_button('LDR @', 'ﮦ → Restore Session', ':RestoreSession<CR>'),
--     create_button(
--         'LDR o c',
--         '漣→ Configuration',
--         ':cd ~/.config/nvim/ | Telescope file_browser cwd=~/.config/nvim/<CR>'
--     ),
-- }
--
-- -- Function to create padding {{{
-- -- @usage = create_padding(number)
-- local function create_padding(number)
--     return { type = 'padding', val = number }
-- end
-- -- }}}
--
-- local config = {
--     layout = {
--         create_padding(1),
--         parts.header,
--         create_padding(1),
--         parts.date,
--         parts.plugins,
--         create_padding(2),
--         parts.buttons,
--     },
--     opts = { margin = 5 },
-- }
--
-- function M:setup()
--     -- Setup highlight
--     vim.cmd('hi AlphaHeader fg=#5c6370')
--     vim.cmd('hi AlphaBody fg=#5c6370')
--     vim.cmd('hi AlphaFooter fg=#5c6370')
--     require('alpha').setup(config)
--     require('util').map('n', '<leader>.', '<cmd>Alpha<CR>')
-- end
--
-- return M
