#!/bin/sh
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
      echo "Invalid option: $1. Please, specify --max, --folder or --compilation options, for example, --max 15 --folder /some/path"
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
