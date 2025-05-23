# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains dotfiles managed through Nix, specifically using nix-darwin for macOS system configuration and home-manager for user-level configuration. The setup targets Apple Silicon (aarch64-darwin) machines.

## Architecture

- **Flake-based**: Uses Nix Flakes for reproducible builds and dependency management
- **Two-layer configuration**:
  - System level (nix-darwin): System packages, macOS settings, Homebrew integration
  - User level (home-manager): User dotfiles, application configs, shell setup
- **Modular structure**:
  - `macos/`: macOS-specific configurations
  - `modules/`: Shared configuration modules for applications

## Key Files

- `flake.nix`: Main entry point defining inputs (dependencies) and configuration
- `build.sh`: Script to build and apply the configuration
- `macos/default.nix`: Main macOS system configuration
- `macos/home.nix`: Home-manager configuration that imports shared modules
- `macos/packages.nix`: User packages definition

## Common Commands

### Building and Applying Configuration

To build and apply the configuration (both system and user level):

```bash
./build.sh
```

This script handles:
1. Building the Nix configuration
2. Applying it with darwin-rebuild
3. Cleaning up temporary files

### Maintenance Commands

Clean up the Nix store when it gets too large:

```bash
sudo nix-collect-garbage -d
```

Show system Nix information:

```bash
nix-shell -p nix-info --run "nix-info -m"
```

Find where an executable is located in the Nix store:

```bash
nix-build -E 'with import <nixpkgs> {}; executable' --no-out-link
```

### Updating the System

To update packages to the latest version:

1. Update flake inputs:
```bash
nix flake update
```

2. Apply the configuration:
```bash
./build.sh
```

## Repository Structure Notes

- Configuration primarily uses Nix expressions
- Homebrew is used as a complementary package manager for macOS-specific applications
- GPG/Yubikey integration is configured for Git signing
- Various developer tools are installed through Nixpkgs