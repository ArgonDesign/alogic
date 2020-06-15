#!/usr/bin/env bash
# Copyright (c) 2020 Argon Design Ltd. All rights reserved.

set -e
set -x

fatal() {
  echo "ERROR: $(basename "$0"): $1" >&2; exit 1;
}

if [ "$TRAVIS_BUILD_STAGE_NAME" = "deps" ]; then
  ##############################################################################
  # Install packages for building dependencies
  ##############################################################################

  sudo apt-get update # Required, as no apt-get used in .travis.yml, meaning
                      # Travis will not do it for us.

  # SymbiYosys dependencies
  sudo apt-get install gperf
  sudo apt-get install libftdi-dev
  sudo apt-get install libboost-program-options-dev
  # Verilator dependencies
  sudo apt-get install libfl-dev
  sudo apt-get install libgoogle-perftools-dev
elif [ "$TRAVIS_BUILD_STAGE_NAME" = "test" ]; then
  ##############################################################################
  # Install packages for running tests
  ##############################################################################

  true # Nothing to do at the moment
else
  ##############################################################################
  # Unknown build stage
  ##############################################################################

  fatal "Unknown build stage: '$TRAVIS_BUILD_STAGE_NAME'"
fi
