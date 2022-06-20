<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-maven-plugin)
[![Javadoc](http://www.javadoc.io/badge/org.eolang/eo-maven-plugin.svg)](http://www.javadoc.io/doc/org.eolang/eo-maven-plugin)

This is Maven plugin for EO.

Here is a simple program that gets a year from command line and tells you
whether it's leap or not:

```
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
        <version><!-- check Maven Central --></version>
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

