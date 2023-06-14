-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("UserLspConfig", {}),
	callback = function(ev)
		-- Enable completion triggered by <c-x><c-o>
		vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

		-- Buffer local mappings.
		-- See `:help vim.lsp.*` for documentation on any of the below functions
		local opts = { buffer = ev.buf }
		vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
		vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
		-- vim.keymap.set('n', '<C-h>', vim.lsp.buf.signature_help, opts)
		vim.keymap.set("n", "<leader>wl", function()
			print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
		end, opts)
		vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, opts)
		vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
		vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
		vim.keymap.set("n", "<leader>rr", vim.lsp.buf.references, opts)
		vim.keymap.set("i", "<C-h>", function()
			vim.lsp.buf.signature_help()
		end, opts)
		vim.keymap.set("n", "<space>f", function()
			vim.lsp.buf.format({ async = true })
		end, opts)
	end,
})

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local util = require("lspconfig.util")
local lspconfig = require("lspconfig")

lspconfig.tsserver.setup({
	capabilities = capabilities,
})
lspconfig.rust_analyzer.setup({
	-- Server-specific settings. See `:help lspconfig-setup`
	settings = {
		["rust-analyzer"] = {},
	},
	capabilities = capabilities,
})
lspconfig.solidity.setup({
	cmd = { "nomicfoundation-solidity-language-server", "--stdio" },
	filetypes = { "solidity" },
	--	root_dir = require("lspconfig.util").find_git_ancestor,
	root_dir = function(fname)
		return require("lspconfig.util").root_pattern(
			"hardhat.config.js",
			"package.json",
			"foundry.toml"
		)(fname) or require("lspconfig.util").path.dirname(fname)
	end,
	single_file_support = true,
	capabilities = capabilities,
})

lspconfig.lua_ls.setup({
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
	capabilities = capabilities,
})

lspconfig.grammarly.setup({
	filetypes = { "markdown", "tex", "text" },
	cmd = {
		"/Users/anajulia/.nvm/versions/node/v16.15.1/bin/grammarly-languageserver",
		"--stdio",
	},
	root_dir = util.find_git_ancestor,
	single_file_support = true,
	handlers = {
		["$/updateDocumentState"] = function()
			return ""
		end,
	},
	init_options = {
		clientId = os.getenv("GRAMMARLY_CLIENT_ID"),
	},
	default_config = {
		root_dir = [[util.find_git_ancestor]],
	},
})

lspconfig.marksman.setup({
	capabilities = capabilities,
})
