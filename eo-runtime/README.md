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

## How to release eo-runtime

Here is some steps to release eo-runtime:

1.To run `.rultor.yml` script in `objectionary/eo`. It is better to do it by [rultor](https://www.yegor256.com/2014/07/24/rultor-automated-merging.html) command in any github comment:
   ```
   @rultor release, tag is `{new-version}`
   ```
   Running this script will update the `gh-pages` git branch. Which contains only `.eo` objects from the `eo-runtime` package and the file required by the script: [objectionary.lst](https://github.com/objectionary/eo/blob/gh-pages/objectionary.lst).
   This script also will create several files in Maven central repository (eg `.jar`, `.pom` etc.).

2.To create a pull request in `objectionary/home` by `pull.sh` script in a separate git branch (the name of the branch doesn't matter):
   ```shell
   ./pull.sh objectionary/eo
   ```
   This will update all `.eo` files in `gh-pages` from `objectionary/eo`. It will also change the corresponding versions of the eo objects (e.g. from the `+version` metadata) in the `objectionary/home` repository.

3.To check and to remove manually either unused or old files. Script didn't do it.

4.To update `eo.version` in `pom.xml` in `objectionary/home`.

5.Finally, all `todo` must be removed manually.