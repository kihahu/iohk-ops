{
  commit  # commit of cardano-sl
}:

with import <nixpkgs> {};
writeScriptBin "set-rev.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  $IO -C .. set-rev cardanosl ${commit}
''
