--require("nvim-tree").setup({
--	sort_by = "case_sensitive",
--	view = {
--		width = 30,
--	},
--	renderer = {
--		group_empty = true,
--	},
--	filters = {
--		dotfiles = true,
--	},
--})
--
--
--vim.g.nvim_tree_side = "left"
--vim.g.nvim_tree_width = 24
--vim.g.nvim_tree_ignore = { ".git", "node_modules", ".cache" }
--vim.g.nvim_tree_auto_open = 0
--vim.g.nvim_tree_auto_close = 0
--vim.g.nvim_tree_quit_on_open = 0
--vim.g.nvim_tree_follow = 1
--vim.g.nvim_tree_indent_markers = 1
--vim.g.nvim_tree_hide_dotfiles = 1
--vim.g.nvim_tree_git_hl = 1
--vim.g.nvim_tree_root_folder_modifier = ":~"
--vim.g.nvim_tree_tab_open = 1
--vim.g.nvim_tree_allow_resize = 1
--

require("neo-tree").setup()

vim.keymap.set("n", "<leader>e", ":NeoTreeShowToggle<CR>")
