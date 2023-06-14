local null_ls = require("null-ls")
local helpers = require("null-ls.helpers")

local lsp_formatting = function(bufnr)
	vim.lsp.buf.format({
		filter = function(client)
			return client.name == "null-ls"
		end,
		bufnr = bufnr,
	})
end

local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

local on_attach = function(client, bufnr)
	if client.supports_method("textDocument/formatting") then
		vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
		vim.api.nvim_create_autocmd("BufWritePre", {
			group = augroup,
			buffer = bufnr,
			callback = function()
				lsp_formatting(bufnr)
			end,
		})
	end
end

null_ls.setup({
	sources = {
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.diagnostics.eslint,
		null_ls.builtins.diagnostics.luacheck.with({
			args = { "--globals vim" },
		}),
		null_ls.builtins.code_actions.eslint,
		null_ls.builtins.completion.spell,
		--	null_ls.builtins.formatting.eslint.with({
		--		filetypes = {
		--			"javascript",
		--"typescript",
		--				"json",
		--				"yaml",
		--				"markdown",
		--			},
		--args = { "--find-config-path", "--config" }
		--		}),
		null_ls.builtins.diagnostics.solhint,
		null_ls.builtins.formatting.prettier.with({
			filetypes = { "javascript", "typescript", "markdown", "md" },
			--args = { "--find-config-path", "--config", "--write" },
			extra_filetypes = { "solidity" },
		}),
		null_ls.builtins.formatting.latexindent.with({
			filetypes = { "tex" },
			extra_args = {
				"-s",
				"-m",
				"$FILENAME",
				"-l",
				".indentconfig.yaml",
			},
			root_dir = function(fname)
				return require("lspconfig").util.find_git_ancestor(fname)
					or vim.fn.expand("~")
			end,
		}),
		-- this was making python crash
		--null_ls.builtins.diagnostics.proselint,
		--null_ls.builtins.code_actions.proselint,
	},
	on_attach = on_attach,
	debug = true,
})

local markdownlint = {
	method = null_ls.methods.DIAGNOSTICS,
	filetypes = { "markdown" },
	-- null_ls.generator creates an async source
	-- that spawns the command with the given arguments and options
	generator = null_ls.generator({
		command = "markdownlint",
		args = { "--stdin" },
		to_stdin = true,
		from_stderr = true,
		-- choose an output format (raw, json, or line)
		format = "line",
		check_exit_code = function(code, stderr)
			local success = code <= 1

			if not success then
				-- can be noisy for things that run often (e.g. diagnostics), but can
				-- be useful for things that run on demand (e.g. formatting)
				print(stderr)
			end

			return success
		end,
		-- use helpers to parse the output from string matchers,
		-- or parse it manually with a function
		on_output = helpers.diagnostics.from_patterns({
			{
				pattern = [[:(%d+):(%d+) [%w-/]+ (.*)]],
				groups = { "row", "col", "message" },
			},
			{
				pattern = [[:(%d+) [%w-/]+ (.*)]],
				groups = { "row", "message" },
			},
		}),
	}),
}

null_ls.register(markdownlint)
