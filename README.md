Build nix flake

```bash
nix build .#darwinConfigurations.darwin.system
```

After first build, rebuild and load changes. Using impure as a workaround until

```bash
git add .
/result/sw/bin/darwin-rebuild switch --flake . --impure 
```

When nix store gets too large.

```bash
sudo nix-collect-garbage -d
```

## Snippets

- Find here a executable is inside nix store with

```
nix-build -E 'with import <nixpkgs> {}; executable' --no-out-link
```

- Nix info

```
nix-shell -p nix-info --run "nix-info -m"
```
