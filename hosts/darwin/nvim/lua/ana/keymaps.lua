vim.api.nvim_set_keymap("v", "p", '"_dP', {})
vim.api.nvim_set_keymap("v", "J", ":m '>+1<CR>gv=gv", {})
vim.api.nvim_set_keymap("v", "K", ":m '<-2<CR>gv=gv", {})
vim.api.nvim_set_keymap("n", "<C-d>", "<C-d>zz", {})
vim.api.nvim_set_keymap("n", "<C-u>", "<C-u>zz", {})
vim.api.nvim_set_keymap("n", "n", "nzzzv", {})
vim.api.nvim_set_keymap("n", "N", "Nzzzv", {})
vim.api.nvim_set_keymap("n", "Q", "<nop>", {})
--vim.api.nvim_set_keymap("n", "s", "<nop>", {})
vim.api.nvim_set_keymap("n", "<leader>f", ":lua vim.lsp.buf.format()<CR>", {})
-- quickfix list
vim.api.nvim_set_keymap("n", "[q", ":cprev<CR>zz", {})
vim.api.nvim_set_keymap("n", "]q", ":cnext<CR>zz", {})
-- location list
vim.api.nvim_set_keymap("n", "<leader>k", ":lnext<CR>zz", {})
vim.api.nvim_set_keymap("n", "<leader>j", ":lprev<CR>zz", {})
vim.api.nvim_set_keymap(
	"n",
	"<leader>s",
	":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>bn",
	":bnext<CR>",
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>bp",
	":bprevious<CR>",
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>bl",
	":Telescope buffers<CR>",
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>bc",
	":bd<CR>",
	{ noremap = true, silent = true }
)
for i = 1, 9 do
	vim.api.nvim_set_keymap(
		"n",
		"<leader>b" .. i,
		":buffer " .. i .. "<CR>",
		{ noremap = true, silent = true }
	)
end
vim.api.nvim_set_keymap(
	"n",
	"<leader>ss",
	":split<CR>",
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"sv",
	":vsplit<CR>",
	{ noremap = true, silent = true }
)
-- If there are any diagnostic messages from the Marksman
-- or Grammarly LSPs, then there will be marks in the far left
-- column.  `E` for error, `W` for warning, etc.  The two
-- keymaps below will cycle through the diagnostic messages.
-- type `]d` to go forward, and `[d` to go backward:
vim.api.nvim_set_keymap(
	"n",
	"]d",
	"<CMD>lua vim.diagnostic.goto_next()<CR>",
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"[d",
	"<cmd>lua vim.diagnostic.goto_prev()<CR>",
	{ noremap = true, silent = true }
)
