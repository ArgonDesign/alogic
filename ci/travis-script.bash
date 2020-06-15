#!/usr/bin/env bash
# Copyright (c) 2020 Argon Design Ltd. All rights reserved.

set -e
set -x

fatal() {
  echo "ERROR: $(basename "$0"): $1" >&2; exit 1;
}

if [ "$TRAVIS_BUILD_STAGE_NAME" = "deps" ]; then
  ##############################################################################
  # Build dependencies
  ##############################################################################

  bash -x setup-symbiyosys
  bash -x setup-verilator
elif [ "$TRAVIS_BUILD_STAGE_NAME" = "test" ]; then
  ##############################################################################
  # Run tests
  ##############################################################################

  sbt compile test:compile test scalafmtCheckAll
else
  ##############################################################################
  # Unknown build stage
  ##############################################################################

  fatal "Unknown build stage: '$TRAVIS_BUILD_STAGE_NAME'"
fi
