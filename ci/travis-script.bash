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

  if [ "$COVERAGE" != 1 ]; then
    sbt clean compile test:compile test scalafmtCheckAll
  else
    sbt clean coverage test coverageReport
    bash <(curl -s https://codecov.io/bash)
  fi
else
  ##############################################################################
  # Unknown build stage
  ##############################################################################

  fatal "Unknown build stage: '$TRAVIS_BUILD_STAGE_NAME'"
fi
