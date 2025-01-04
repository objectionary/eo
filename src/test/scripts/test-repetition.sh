#!/bin/sh
# The MIT License (MIT)
#
# Copyright (c) 2016-2025 Objectionary.com
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# This script runs the maven tests of the eo component several times
# to check for flaky tests and to find concurrency issues.
# Usage ./test-repetition.sh --max 15 --folder /some/path
set -x
# Initialize variables
max="10"
folder="../eo-runtime"
compilation=false
# Process command-line options
while [ $# -gt 0 ]
do
  case $1 in
    --max)
      shift
      max=$1
      ;;
    --folder)
      shift
      folder=$1
      ;;
    --compilation)
      shift
      compilation=$1
      ;;
    *)
      echo "Invalid option: $1. Please, specify --max, --folder or --compilation options, \
for example, --max 15 --folder /some/path"
      exit 1
      ;;
  esac
  shift
done
# Print the values of the variables
printf "Number of iterations is %s\n" "$max"
printf "Path to the testable module is %s\n" "$folder"
set -e
# Go to testable folder
cd "$folder"
# Clean the test classes to avoid caching issues and prepare testing environment
# without running the tests
mvn clean install -Pqulice -DskipTests -DskipITs -Dinvoker.skip=true
# Run the tests several times
for i in $(seq 1 "$max")
do
  echo "Test repetition #$i of $max"
  if [ "$compilation" = true ]; then
    echo "Compiling the module and running the tests"
    MAVEN_OPTS=-Dorg.slf4j.simpleLogger.showThreadName=true mvn test -e --batch-mode
  else
    echo "Running the tests"
    MAVEN_OPTS=-Dorg.slf4j.simpleLogger.showThreadName=true mvn surefire:test -e --batch-mode
  fi
done
