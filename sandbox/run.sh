#!/bin/bash
object_to_run=$1
shift
time java -cp target/classes:target/eo-runtime.jar -Xss40m  org.eolang.core.Main sandbox.${object_to_run:-app} "$@"
