Build the nix flake.

```bash
nix build .#darwinConfigurations.darwin.system
```

After first build, rebuild and load changes.

```bash
git add .
./result/sw/bin/darwin-rebuild switch --flake .#darwin
```

When nix store gets too large.

```bash
sudo nix-collect-garbage -d
```
