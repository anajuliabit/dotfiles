vim.g.copilot_no_tab_map = true
vim.api.nvim_set_keymap(
	"i",
	"<C-a>",
	'copilot#Accept("\\<CR>")',
	{ silent = true, expr = true }
)
vim.api.nvim_set_keymap(
	"i",
	"<C-j>",
	"copilot#Previous()",
	{ silent = true, expr = true }
)
vim.api.nvim_set_keymap(
	"i",
	"<C-k>",
	"copilot#Next()",
	{ silent = true, expr = true }
)
vim.api.nvim_set_keymap(
	"i",
	"<C-d>",
	"copilot#Dismiss()",
	{ silent = true, expr = true }
)
--   vim.defer_fn(function()
--     vim.api.nvim_set_hl(0, "CopilotSuggestion", { fg = "#888888" })
--   end, 1000)
vim.cmd([[
 				let g:copilot_enable = 1
 				let g:copilot_filetypes = {
 						\ '*': v:true,
 						\ 'markdown':v:true,
 						\ 'yaml': v:true,
 						\ 'lua': v:true,
 						\ 'gitcommit': v:true,
                        \ 'latex': v:true,
                        \ 'tex': v:true,
 						\ "TelescopePrompt": v:false,
 							\ }

 				" imap <silent><script><expr> <C-e> copilot#Accept('\<CR>')
 						]])
