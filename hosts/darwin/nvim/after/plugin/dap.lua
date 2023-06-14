--local status_ok, dap_vscode_js = pcall(require, "dap-vscode-js")
--if not status_ok then
--	return
--end
--
--dap_vscode_js.setup({
--	debugger_path = vim.fn.stdpath("data")
--		.. "/mason/packages/js-debug-adapter",
--	debugger_cmd = { "js-debug-adapter" },
--	adapters = {
--		"pwa-node",
--		"pwa-chrome",
--		"pwa-msedge",
--		"node-terminal",
--		"pwa-extensionHost",
--	},
--})

local dap = require("dap")

dap.adapters["pwa-node"] = {
	type = "server",
	host = "localhost",
	port = "${port}",
	executable = {
		command = vim.fn.stdpath("data") .. "/mason/bin/js-debug-adapter",
		args = { "${port}" },
	},
}
dap.configurations.javascript = {
	{
		name = "Debug Tests",
		type = "pwa-node",
		request = "launch",
		args = {
			"${file}",
			"--config",
			"./hardhat.config.js",
			"--show-stack-traces",
		},
		sourceMaps = true,
		protocol = "inspector",
		cwd = "${workspaceFolder}",
		console = "integratedTerminal",
		runtimeArgs = { "${workspaceFolder}/node_modules/.bin/hardhat", "test" },
		rootPath = "${workspaceFolder}",
		--runtimeExecutable = "node",
		internalConsoleOptions = "neverOpen",
		--outFiles = { "${workspaceFolder}/**/*.js", "**/@ethersproject/**/*.js" },
		skipFiles = { "<node_internals>/**", "node_modules" },
		port = 8123,
	},
}

local dapui = require("dapui")
dapui.setup({
	controls = {
		element = "console",
		enabled = true,
	},
	layouts = {
		{
			elements = {
				{
					id = "console",
					size = 1,
				},
			},
			position = "left",
			size = 60,
		},
		{
			elements = {
				{
					id = "scopes",
					size = 0.25,
				},
				{
					id = "breakpoints",
					size = 0.25,
				},
				{
					id = "stacks",
					size = 0.25,
				},
				{
					id = "watches",
					size = 0.25,
				},
			},
			position = "bottom",
			size = 10,
		},
	},
})

dap.listeners.after.event_initialized["dapui_config"] = function()
	dapui.open()
end
dap.listeners.before.event_exited["dapui_config"] = function()
	dapui.close()
end

vim.api.nvim_set_keymap(
	"n",
	"<leader>dc",
	":lua require('dap').continue()<CR>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>dt",
	":lua require('dap').toggle_breakpoint()<CR>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>do",
	":lua require('dap').step_over()<CR>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>di",
	":lua require('dap').step_into()<CR>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>du",
	":lua require('dapui').toggle()<CR>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>dd",
	":lua require('dap').step_out()<CR>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"<leader>db",
	":lua require('dap').set_breakpoint(vim.fn.input('Condition:'))<CR>",
	{}
)
