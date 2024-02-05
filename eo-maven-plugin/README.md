<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-maven-plugin)
[![Javadoc](http://www.javadoc.io/badge/org.eolang/eo-maven-plugin.svg)](http://www.javadoc.io/doc/org.eolang/eo-maven-plugin)

This is Maven plugin for EO.

Here is a simple program that gets a year from command line and tells you
whether it's leap or not:

```eo
[args...] > main
  [y] > leap
    or. > @
      and.
        eq. (mod. y 4) 0
        not. (eq. (mod. y 100) 0)
      eq. (mod. y 400) 0
  QQ.io.stdout > @
    QQ.txt.sprintf
      "%d is %sa leap year!"
      (args.get 0).as-int > year!
      if. (leap year:y) "" "not "

```

In order to compile this program, put it into `src/main/eo/main.eo` and then
create a file `pom.xml` with this content (it's just a sample):

```xml
<project>
  [...]
  <build>
    <plugins>
      <plugin>
        <groupId>org.eolang</groupId>
        <artifactId>eo-maven-plugin</artifactId>
        <version>0.35.1</version>
        <executions>
          <execution>
            <goals>
              <goal>register</goal>
              <goal>assemble</goal>
              <goal>transpile</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
      <plugin>
        <groupId>org.codehaus.mojo</groupId>
        <artifactId>exec-maven-plugin</artifactId>
        <executions>
          <execution>
            <phase>test</phase>
            <goals>
              <goal>java</goal>
            </goals>
          </execution>
        </executions>
        <configuration>
          <mainClass>org.eolang.Main</mainClass>
          <arguments>
            <argument>main</argument>
            <argument>2008</argument>
          </arguments>
        </configuration>
      </plugin>
    </plugins>
  </build>
</project>
```

Then, you just run `mvn clean test` (you will need [Maven 3.3+](https://maven.apache.org/))
and the `.eo` file will be parsed to `.xml` files, transformed to `.java` files,
and then compiled to `.class` files. You can see them all in the `target` directory.
You will need Java 8+.

## How it Works?

The entire process of turning an `.eo` program into an executable
binary code consists of a few high-level steps, which must be done
one after another:

  * **Parsing**.
    It's done by the `org.eolang.parser.EoSyntax` class in the `eo-parser` module. It takes
    the source code in a plain text format and parses into XML document,
    using [ANTLR4](https://www.antlr.org/) and [Xembly](https://www.xembly.org).
    The output of the parser you can find in the `target/eo/parse` directory.
    Parsed objects which are versioned (normally pulled from 
    [Objectionary](https://github.com/objectionary/home)) are cached in `.eo/parsed` folder. 

  * **Optimization**.
    There are a number of [XSL transformations](https://en.wikipedia.org/wiki/XSLT)
    that need to be done
    with the XML document in order to make it ready for compilation.
    Each transformation has its own `.xsl` file in the `eo-parser` directory.
    The class `org.eolang.parser.Program` is responsible for making XSLT
    transformations and the entire list of them is stored in the
    `org.eolang.parser.Pack` class. Some of XLST files are sanity checks (or linters).
    The output of each transformation you can find in the `target/eo/optimize` directory.

  * **Compilation**.
    The class `org.eolang.maven.TranspileMojo` in the `eo-maven-plugin` module is responsible
    for taking parsed and optimized XMIRs and then transforming
    the XML document into a collection of `.java` files. There are a number
    of transformations that do this, they all exist in `.xsl` files.
    The output of this step you can find in the `target/generated-sources` directory.

There is also a module called `eo-runtime`, which includes both `.eo` and `.java` code
for most popular and important objects that any of you will need in order
to write even a simple EO program. There are objects like `string`, `int`, `sprintf`,
`stdout`, and so on. By the way, you may want to contribute there by creating new objects.

## Objectionary objects cache

If any external object is used in EO program it will be pulled from [Objectionary home](https://github.com/objectionary/home).
By default, during compilation the plugin will check local cache (`~/.eo`) for required objects
and only download (and cache) them in case they are not found locally.

This behaviour can be changed to always download objects from remote by providing 
Maven option `-U` (see [Maven CLI docs](https://maven.apache.org/ref/3.1.0/maven-embedder/cli.html)):

```shell
mvn -U clean install
```

If you want to build your project or run your tests which use eo-maven-plugin, you can change stack size for your purposes by stack-size option:

```shell
mvn clean install -Pqulice -Dstack-size=1M
```
where 1M is size of stack. By default stack-size = 256M in eo-maven-plugin, maximum size is 1G.

## How to run Integration Tests only

If you want to run a specific integration test without waiting until other unit or integration tests are executed you need to go to `eo-maven-plugin` 
directory and execute the next command:

```shell
mvn clean integration-test invoker:run -Dinvoker.test=fibonacci -DskipTests
```

Here `fibonacci` is the name of the desired integration test, `-DskipTests` is used in order to skip `eo-maven-plugin` unit tests.

## How to disable Integration Tests

It is sometime necessary to temporary disable the integration tests (for example for introducing braking changes into plugin or EO runtime). 
This can be achieved by disabling `maven-invoker-plugin` execution within `eo-maven-plugin/pom.xml`:
```xml
<plugins>
  ...
  <plugin>
    <artifactId>maven-invoker-plugin</artifactId>
    <version>0.35.1</version>
    <configuration>
        <skipInstallation>true</skipInstallation>
        <skipInvocation>true</skipInvocation>
    </configuration>
  </plugin>
  ...
</plugins>  
```
