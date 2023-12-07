Build nix flake

```bash
nix build .#darwinConfigurations.darwin.system
```

After first build, rebuild and load changes. Using impure as a workaround until

```bash
git add .
./result/sw/bin/darwin-rebuild switch --flake .#darwin --impure
```

When nix store gets too large.

```bash
sudo nix-collect-garbage -d
```

## Bugs

https://github.com/NixOS/nix/issues/3261

nix command crash with:

```
error (ignored): unable to download [...]: Problem with the SSL CA cert (path? access rights?) (77)
```

solution:

```
sudo launchctl setenv NIX_SSL_CERT_FILE /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt
sudo launchctl kickstart -k system/org.nixos.nix-daemon
nix-shell -p nix-info --run "nix-info -m"
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
