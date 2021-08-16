{ pkgs ? import <nixpkgs> { } }:

let
  # https://hub.docker.com/r/utdemir/ghc-musl
  imageName = "utdemir/ghc-musl";
  imageTag = "v20-ghc8104";

  # Values gotten from the output of:
  #
  #   nix-prefetch-docker utdemir/ghc-musl v20-ghc8104
  dockerImage = pkgs.dockerTools.pullImage {
    inherit imageName;
    imageDigest =
      "sha256:bf0db3ae523dd77b89fcaadee49010cfca83accf84d2070d4ff790557940e8d8";
    sha256 = "17fa030ja71xwhzll43jbb5zbchb9qrv6kpxv1c5dh7j2if92xbq";
    finalImageName = imageName;
    finalImageTag = imageTag;

    os = "linux";
    arch = "x86_64";
  };

in {
  inherit dockerImage;

  loadDockerImageBashScript = pkgs.writeScript "load-docker-image.bash" ''
    #!/usr/bin/env bash

    set -eo pipefail

    if (( BASH_VERSINFO[0] > 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4 ) ))
    then
        # Treat unset variables and parameters as an error when expanding.  This
        # wasn't very reliable in older Bash versions, hence the version check.
        set -u

        # Command substitutions will inherits the value of the `set -e`, i.e.
        # `set -o errexit`.
        #
        # Available since Bash 4.4, this is actually something Bash does in POSIX
        # mode without it needed to be told.
        shopt -s inherit_errexit
    fi

    if [[ -n "''${DEBUGGING_MODE:-}"  ]]; then
        # This will cause Bash to print commands before executing them.
        set -x
    fi

    # Usage:
    #
    #   main [--full-image-name]
    function main() {
        if (( $# )); then
            case "$1" in
                --full-image-name)
                    echo "${imageName}:${imageTag}"
                    ;;

                *)
                    exit 1
                    ;;
            esac
        else
          echo "Loading docker image: ${dockerImage}"
          docker load --input "${dockerImage}"
        fi
    }

    main "$@"
  '';
}
