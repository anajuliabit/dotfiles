## Setup 

Build nix flake

```bash
nix build .#darwinConfigurations.darwin.system
```

After first build, rebuild and load changes.
Using impure as a workaround until I migrate nvim conf to nix (loads ~/.config/nvim-bkp)

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

## Todo

Programs:

- [ ] Braver
- [x] Spotify
- [x] Discord
- [ ] Yubikey manager

Terminal:

- [x] zsh
- [x] Tmux
- [ ] Copilot

Editor: 

- [x] nvim - temporarily solution
- [ ] emacs

Mac:

- [x] Yabai
- [x] Skhd
- [ ] Spacebar
