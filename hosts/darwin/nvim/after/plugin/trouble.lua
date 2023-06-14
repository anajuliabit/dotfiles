vim.keymap.set("n", "<leader>t", "<cmd>TroubleToggle<cr>")
vim.keymap.set(
	"n",
	"<leader>tw",
	"<cmd>TroubleToggle workspace_diagnostics<cr>"
)
vim.keymap.set("n", "<leader>td", "<cmd>TroubleToggle document_diagnostics<cr>")
vim.keymap.set("n", "<leader>tq", "<cmd>TroubleToggle quickfix<cr>")
vim.keymap.set("n", "<leader>tl", "<cmd>TroubleToggle loclist<cr>")

require("trouble").setup()
