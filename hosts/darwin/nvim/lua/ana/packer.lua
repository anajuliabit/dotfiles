local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath("data")
		.. "/site/pack/packer/start/packer.nvim"
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({
			"git",
			"clone",
			"--depth",
			"1",
			"https://github.com/wbthomason/packer.nvim",
			install_path,
		})
		vim.cmd([[packadd packer.nvim]])
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

return require("packer").startup(function(use)
	use("wbthomason/packer.nvim")
	use("nvim-treesitter/playground")
	use("theprimeagen/harpoon")
	use("mbbill/undotree")
	use("tpope/vim-fugitive")
	use("nvim-tree/nvim-web-devicons")
	use({
		"williamboman/mason.nvim",
		run = function()
			local mason_update =
				require("mason.install").update({ with_sync = true })
			mason_update()
		end,
	})
	use({
		"nvim-telescope/telescope.nvim",
		tag = "0.1.1",
		-- or                            , branch = '0.1.x',
		requires = { { "nvim-lua/plenary.nvim" } },
	})
	use({
		"williamboman/mason-lspconfig.nvim",
		run = function()
			require("mason-lspconfig").setup()
		end,
	})

	use("neovim/nvim-lspconfig")
	use({
		"rose-pine/neovim",
		as = "rose-pine",
	})
	use({
		"catppuccin/nvim",
		as = "catppuccino",
	})
	use({
		"nvim-treesitter/nvim-treesitter",
		run = function()
			local ts_update =
				require("nvim-treesitter.install").update({ with_sync = true })
			ts_update()
		end,
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		requires = { "nvim-lua/plenary.nvim" },
	})
	use("folke/trouble.nvim")
	use({
		"mfussenegger/nvim-dap",
		requires = {
			"theHamsta/nvim-dap-virtual-text",
		},
	})
	--	use({
	--		"microsoft/vscode-js-debug",
	--		opt = true,
	--		run = "npm install --legacy-peer-deps && npm run compile",
	--	})
	use("github/copilot.vim")
	use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })
	use("folke/neodev.nvim")
	if packer_bootstrap then
		require("packer").sync()
	end
	use({
		"iamcco/markdown-preview.nvim",
		run = "cd app && npm install",
		setup = function()
			vim.g.mkdp_filetypes = { "markdown" }
		end,
		ft = { "markdown" },
	})
	use("artempyanykh/marksman")
	use("theHamsta/nvim-dap-virtual-text")
	use({
		"renerocksai/telekasten.nvim",
		requires = { "nvim-telescope/telescope.nvim" },
	})
	use({
		"lervag/vimtex",
		ft = { "tex" },
		lazy = false,
		opts = { patterns = { "*.tex" } },
		config = function()
			vim.g.vimtex_view_method = "skim"
			vim.g.vimtex_view_skim_sync = 1
			vim.g.vimtex_view_skim_activate = 1
			vim.g.vimtex_mappings_enabled = true
			vim.g.tex_flavor = "latex"
			--vim.g.vimtex_quickfix_mode = 2
			--vim.g.vimtex_quickfix_open_on_warning = 0
			--vim.g.vimtex_compiler_method = "tectonic"
			--vim.g.vimtex_view_general_options = "-r @line @pdf @tex"
			--vim.g.vimtex_view_general_viewer =
			--		"/Applications/Skim.app/Contents/SharedSupport/displayline"
		end,
	})
	use({
		"donRaphaco/neotex",
		ft = { "tex" },
	})
	use({
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v2.x",
		requires = {
			"nvim-lua/plenary.nvim",
			"MunifTanjim/nui.nvim",
		},
	})
	--	use("jceb/vim-orgmode")
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-nvim-lsp")
	use("saadparwaiz1/cmp_luasnip")
	use("L3MON4D3/LuaSnip")
	use("rafamadriz/friendly-snippets")
	use("simnalamburt/vim-mundo")
	use("nvim-telescope/telescope-media-files.nvim")
	use("nvim-lualine/lualine.nvim")
end)
