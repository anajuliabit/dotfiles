{ pkgs, ... }:

{
    xdg.configFile."nvim".source = "/Users/anajulia/.config/nvim-bkp";

    home.packages = with pkgs; [
      stylua
      nodePackages_latest.prettier
      # black
      # alejandra
      # shfmt
      # rustfmt is provided by rust-overlay
  
      # selene
      # nodePackages_latest.eslint_d
      shellcheck
      # statix
  
      # lua-language-server
      nodePackages_latest.typescript-language-server
      nodePackages_latest.bash-language-server
  
      gcc
    ];

    programs.neovim = {
         package = pkgs.neovim-unwrapped; 
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
       # plugins = with pkgs.vimPlugins; [ 
       #     plenary-nvim
       #     gruvbox-material
       #     vim-nix
       #     {
       #         plugin = nvim-tree-lua;
       #         config = "lua require('nvim-tree').setup()";
       #     }
       #     nvim-web-devicons
       #     {
       #         plugin = impatient-nvim;
       #         config = "lua require('impatient')";
       #     }
       #     {
       #         plugin = lualine-nvim;
       #         config = "lua require('lualine').setup()";
       #     }
       #     {
       #         plugin = telescope-nvim;
       #         config = "lua require('telescope').setup()";
       #     }
       #     {
       #         plugin = indent-blankline-nvim;
       #         config = "lua require('indent_blankline').setup()";
       #     }
       #     {
       #         plugin = nvim-autopairs;
       #         config = "lua require('nvim-autopairs').setup()";
       #     }
       #     {
       #         plugin = nvim-colorizer-lua;
       #         config = "lua require('colorizer').setup()";
       #     }
       #     {
       #         plugin = gitsigns-nvim;
       #         config = "lua require('gitsigns').setup()";
       #     }
       #     {
       #         plugin = nvim-lspconfig;
       #         config = ''
       #             lua << EOF
       #             local capabilities = require('cmp_nvim_lsp').default_capabilities()
       #             require('lspconfig').rust_analyzer.setup{
       #                 capabilities = capabilities
       #             }
       #             require('lspconfig').sumneko_lua.setup{
       #                 capabilities = capabilities
       #             }
       #             require('lspconfig').rnix.setup{
       #                 capabilities = capabilities
       #             }
       #             EOF
       #         '';
       #     }
       #     {
       #         plugin = nvim-cmp;
       #         config = builtins.readFile ./plugins/cmp.lua;
       #     }
       #     luasnip
       #     friendly-snippets
       #     cmp_luasnip
       #     cmp-nvim-lsp
       #     cmp-buffer
       #     cmp-path
       #     cmp-cmdline
       #     {
       #         plugin = nvim-treesitter;
       #         config = ''
       #             lua << EOF
       #             require('nvim-treesitter.configs').setup {
       #                 highlight = {
       #                     enable = true,
       #                     additional_vim_regex_highlighting = false,
       #                 },
       #             }
       #             EOF
       #         '';
       #     }
       #     {
       #         plugin = comment-nvim;
       #         config = "lua require('Comment').setup()";
       #     }
       #     {
       #         plugin = which-key-nvim;
       #         config = ''
       #             lua << EOF
       #             vim.o.timeout = true
       #             vim.o.timeoutlen = 300
       #             require("which-key").setup({ })
       #             EOF
       #         '';
       #     }
       # ];
       # extraLuaConfig = builtins.readFile ./init.lua;
    };
}
