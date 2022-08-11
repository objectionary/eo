<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

This is a simple example a program written entirely in EO.
You can use the files from this directory as a template.
The only change you will have to do is setting
the right versions the `pom.xml`. Use the latest version
visible in this badge:

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-maven-plugin)

Just copy these files to your local directory, change the
version in `pom.xml` and run `mvn test` (it is assumed that
you have [Maven 3.3+](https://maven.apache.org/) installed).

If everything works fine, you will see the `SUCCESS` message
in the console. The files generated during parsing, optimization,
and compilation are available in `target/eo` directory. This directory
will be created by Maven automatically.
