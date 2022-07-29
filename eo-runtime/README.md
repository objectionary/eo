<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-runtime.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-runtime)
[![Javadoc](http://www.javadoc.io/badge/org.eolang/eo-runtime.svg)](http://www.javadoc.io/doc/org.eolang/eo-runtime)

This is runtime for EO.

## How to run tests
You can run tests just like this:
```
mvn clean test
```

If you want to run tests separately, you can run:
```
mvn clean test -Dtest="{test}"
```
Here the variable `{test}` is the full name of the test, or several test names separated by commas (`,`). 

For example, this `EO` test:
```
[] > prints-itself
  assert-that > @
    length.
      as-phi $
    $.greater-than 0
```

you can run just like this:
```
mvn clean test -Dtest="EOorg.EOeolang.EOprints_itselfTest"
```
