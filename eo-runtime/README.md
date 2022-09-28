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

### Note on logging within tests

By default, logging within tests is limited since it
produces quite extensive logs which affect performance.
However, for debugging/analysis purposes extended (debug) logs can be enabled.
For that both JUL and Logback levels need to be set to `FINE`. This is 
accomplished by amending the following two files:  
_test\resources\jul.properties_:
```
.level=FINE
```
_test\resources\logback.xml_:
```
<logger name="org.eolang.Dataized" level="FINE"/>
<logger name="org.eolang.PhDefault" level="FINE"/>
```
For the latter `FINE` level must be set to desired object. `Dataized` will
produce extensive logging for dataization process and `PhDefault` for 
attributes retrieval process.  
By default, dataization will be logged up until nesting level 3.
If for some reason deeper dataization log is required it can be
achieved by jvm property `-Dmax.dataization.log=<N>`
where `N` is an integer representing desired nesting level to log.
