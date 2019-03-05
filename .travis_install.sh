#! /usr/bin/env bash

set -ev

case $IDRIS_VERSION in
     git)
         pushd .
         git clone https://github.com/idris-lang/Idris-dev.git /tmp/Idris-dev
         cd /tmp/Idris-dev
         stack --no-terminal --install-ghc install --flag idris:-FFI --flag idris:-GMP
         popd
         ;;
     stackage)
         travis_wait 30 stack install --resolver lts-12.26 idris
         ;;
     *)
         echo '$IDRIS_VERSION unspecified'
         ;;
esac
