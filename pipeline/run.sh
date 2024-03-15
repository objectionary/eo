#!/bin/bash

set -euo pipefail

if ! [ -d node_modules ]; then npm i; fi

export LC_ALL=C.UTF-8

shopt -s extglob
shopt -s expand_aliases

alias eo="npx eoc --parser=0.35.11"

DATA_DIR=".data"

function prepare_directory {
    printf "\nClean %s directory\n\n" "$DATA_DIR"

    rm -rf "$DATA_DIR"

    printf "\nGenerate EO test files\n\n"

    stack run transform-eo-tests
}

function enter_directory {
    printf "\nEnter the %s directory\n\n" "$DATA_DIR"

    mkdir -p "$DATA_DIR"
    cd "$DATA_DIR"
}

function tests_without_normalization {
    printf "\nConvert EO to PHI\n\n"

    mkdir -p phi
    cd eo
    eo clean
    eo phi
    cp -r .eoc/phi/!(org) ../phi
    cd ..


    printf "\nConvert PHI to EO without normalization\n\n"

    mkdir -p eo-not-normalized
    cd phi
    cp -r ../eo/.eoc .
    eo unphi --tests
    cp -r .eoc/unphi/!(org) .eoc/2-optimize
    eo print
    cp -r .eoc/print/!(org) ../eo-not-normalized
    cd ..

    printf "\nTest EO without normalization\n\n"

    cd eo-not-normalized
    eo test
    cd ..
}

prepare_directory
enter_directory
tests_without_normalization
