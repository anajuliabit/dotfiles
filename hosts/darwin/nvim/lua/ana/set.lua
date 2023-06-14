vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.mapleader = " "

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.statusline = "%{FugitiveStatusline()}"
vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.o.clipboard = "unnamedplus"

vim.cmd([[set tags+=tags]])
vim.cmd([[
  autocmd BufWritePost *.tex silent! !make format
]])

-- Give me some fenced codeblock goodness
vim.g.markdown_fenced_languages = {
	"javascript",
	"typescript",
	"lua",
	"vim",
	"solidity",
	"rust",
}
