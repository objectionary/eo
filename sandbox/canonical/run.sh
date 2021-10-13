#!/bin/bash

java -cp target/classes:target/eo-runtime.jar org.eolang.Main sandbox.app "$@"
