local telescope = require("telescope")
local builtin = require("telescope.builtin")

local option = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>ff", builtin.find_files, option)
vim.keymap.set("n", "<leader>fg", builtin.live_grep, option)
vim.keymap.set("n", "<leader>fa", function()
	require("telescope.builtin").live_grep({
		file_ignore_patterns = {}, -- Override the default ignore patterns
	})
end, option)
-- Find word under cursor
vim.keymap.set("n", "<leader>fc", builtin.grep_string, option)
--find line on buffer
vim.keymap.set("n", "<leader>fl", builtin.current_buffer_fuzzy_find, option)
--vim.keymap.set("n", "<leader>fs", function()
--	builtin.grep_string({ search = vim.fn.input("Grep > ") })
--end, option)

telescope.setup({
	defaults = {
		winblend = 0,
		layout_config = {
			prompt_position = "top",
			width = 0.75,
			height = 0.5,
		},
		file_ignore_patterns = {
			"dist",
			"node_modules",
			"submodules",
			"addresses",
		},
	},
	pickers = {
		find_files = {
			theme = "dropdown",
			--previewer = false,
			layout_config = {
				width = 0.5,
				height = 0.5,
			},
		},
	},
	extensions = {},
	media_files = {
		filetypes = { "png", "mp4", "jpg", "jpeg", "pdf" },
		find_cmd = "rg",
	},
})

telescope.load_extension("media_files")
