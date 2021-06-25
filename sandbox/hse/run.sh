#!/bin/bash
object_to_run=$1
shift
time java -cp target/classes:target/eo-runtime.jar -Xss40m  org.eolang.hse.core.Main sandbox.hse.${object_to_run:-app} "$@"
