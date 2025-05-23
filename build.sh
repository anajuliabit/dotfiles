#!/bin/sh -e
VERSION=1.0

GREEN='\033[1;32m'
RED='\033[1;31m'
CLEAR='\033[0m'

FLAKE="default"
SYSTEM="darwinConfigurations.$FLAKE.system"

export NIXPKGS_ALLOW_UNFREE=1

# Navigate to the directory of this script
cd $(dirname $(readlink -f $0))

echo "${GREEN}Starting...${CLEAR}"
nix --experimental-features 'nix-command flakes' build .#$SYSTEM --impure --show-trace $@

echo "${GREEN}Switching to new generation...${CLEAR}"
sudo ./result/sw/bin/darwin-rebuild switch --flake .#$FLAKE --impure $@

echo "${GREEN}Cleaning up...${CLEAR}"
rm -rf ./result

echo "${GREEN}Done${CLEAR}"
