<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-maven-plugin)

You can play with EOLANG here, in a few simple steps:

First, clone this repo to your local machine and go
to the `sandbox` directory (you will need
[Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
installed):

```bash
$ git clone https://github.com/cqfn/eo.git
$ cd eo/sandbox
```

`eo/app.eo` is the entrypoint of the program. `app` object will be "evaluated", when the program is run, so modify it to make changes to the program. Then, proceed with compilation and run the program in either docker or natively on your system.

# Compile & run

## In docker

If you don't have Maven or JDK installed on your system, you can compile and run the application in docker. You must have [Docker](https://docs.docker.com/get-docker/) and [docker-compose](https://docs.docker.com/compose/install/) installed to use this method.

### Compile

Compile the sources in `eo/` using maven in docker container. The output of the compilation will be in the `target/` directory.

```bash
docker-compose -p eo-lang run maven
```

> **Overriding the default command**
> 
> By default, the container's command is `mvn compile`. You are free to override it, when running from terminal by appending the command you want to run after the service name, e.g. to run `mvn clean compile` use:
> ```bash
> docker-compose -p eo-lang run maven mvn clean compile
> ```

### Run

After the compilation, the resulting Java program can be run in docker via:

```bash
docker-compose -p eo-lang run app
```

> **Passing command line arguments**
> 
> It is possible to pass command line arguments to the program by appending them after the container name, e.g.:
> ```bash
> docker-compose -p eo-lang run app "Command line arguments go here" 10
> ```

## Natively (via system maven and Java)

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

Should work. If it doesn't, [submit an issue](https://github.com/cqfn/eo/issues),
we will fix it.

Then, you can modify `*.eo` files, run `mvn compile` to compile them
again and `run.sh` to run it again.

[Have fun!](https://www.elegantobjects.org)
