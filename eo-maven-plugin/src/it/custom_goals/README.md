<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

The purpose of `custom-goals` integration test is to check partial compilation
of EO programs. We have to be sure that the plugin correctly performs particular
set of goals in order to compile a program. We just check that for a
particular set of goals the particular set of artifacts and folders will be
compiled and created. The idea is to use only specific goals of the plugin rather
than entire compilation process. The full list of steps that will be performed you
can check in the [pom.xml](pom.xml) in `eo-maven-plugin` description:

```mvn
<goals>
  <goal>register</goal>
  <goal>parse</goal>
  <goal>optimize</goal>
  <goal>shake</goal>
  <goal>discover-foreign</goal>
  <goal>probe</goal>
  <goal>pull</goal>
  <goal>resolve</goal>
</goals>
```

The test was written to help to fix
the [bug](https://github.com/objectionary/eo/issues/1784),
that was found during partial compilation.

The entire compilation process with all possible steps described in that
comprehensive [article](https://www.yegor256.com/2021/10/21/objectionary.html).
___
You can use the files from this directory as a template for other integration
tests. The only change you will have to do is setting
the right versions the `pom.xml`.
Use the latest version visible in this badge:

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-maven-plugin)

Just copy these files to your local directory, change the
version in `pom.xml` and run `mvn test` (it is assumed that
you have [Maven 3.3+](https://maven.apache.org/) installed).

If everything works fine, you will see the `SUCCESS` message
in the console. The files generated during parsing, optimization,
and compilation are available in `target/eo` directory. This directory
will be created by Maven automatically.
