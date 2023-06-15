require("ana")

--vim.diagnostic.config({
--	-- Grammarly default is to show the issue at the end
--	-- of the line (virtual text), I prefer it in a float,
--	-- so turn off the virtual text
--	virtual_text = false,
--	update_in_insert = true,
--	underline = true,
--	severity_sort = true,
--	float = {
--		focusable = false,
--		style = "minimal",
--		border = "rounded",
--
--		-- show the name of the LSP
--		source = "always",
--
--		-- set header to nothing to avoid
--		-- a line reading "Diagnostics:"
--		header = "",
--
--		-- Set prefix to nothing to avoid
--		-- numbering multiple errors on a
--		-- phrase
--		prefix = "",
--	},
--})

if vim.fn.has("persistent_undo") == 1 then -- check if your vim version supports it
	vim.opt.undofile = true -- turn on the feature
	vim.fn.mkdir(vim.fn.expand("$HOME") .. "/.vim/undo", "p")
	vim.opt.undodir = vim.fn.expand("$HOME") .. "/.vim/undo" -- directory where the undo files will be stored
end
