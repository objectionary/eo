<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-maven-plugin)

You can play with EOLANG here, in a few simple steps:

First, clone this repo to your local machine and go
to the `sandbox` directory (you will need
[Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
installed):

```bash
$ git clone https://github.com/yegor256/eo.git
$ cd eo/sandbox
```

Then, compile the code (you will need
[Maven 3.3+](https://maven.apache.org/)
and [Java SDK 8+](https://www.java.com/en/download/) installed):

```bash
$ mvn compile
```

Intermediary `*.xml` files will be generated in the `target` directory (it will
be created). Also, there will be `*.java` and `*.class` files. Feel free to analyze
them: EO is parsed into XML, then translated to Java, and then compiled
by Java SDK to Java bytecode. Finally, just run the bytecode program through JRE:

```bash
$ ./run.sh 9
9th Fibonacci number is 34
```

Should work. If it doesn't, [submit an issue](https://github.com/yegor256/eo/issues),
we will fix it.

Then, you can modify `*.eo` files, run `mvn compile` to compile them
again and `run.sh` to run it again.

[Have fun!](https://www.elegantobjects.org)
