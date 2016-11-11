<img src="http://cf.jare.io/?u=http%3A%2F%2Fwww.yegor256.com%2Fimages%2Fbooks%2Felegant-objects%2Fcactus.svg" width="125px" height="138px" />

EO is an object-oriented programming language. It's still a prototype.
It's the future of OOP. Please contribute!

If you want to contribute, please join our
[Gitter chat](https://gitter.im/yegor256/elegantobjects) first.

These things we don't tolerate:

  * static/class methods or attributes
  * classes (only types and objects)
  * implementation inheritance
  * mutability
  * NULL
  * global variables/procedures
  * reflection
  * constants
  * type casting
  * scalar types
  * garbage collection
  * operators (`for`, `while`, `if`, etc)

These things we want to build in:

  * static analysis
  * continuous integration
  * build automation
  * aspects (AOP)
  * logging
  * unit testing
  * versioning
  * concurrency
  * object metadata
  * persistence
  * transactions
  * licensing
  * artifact repositories

These things we are not sure about (please, help us):

  * generics

We want EO to be compilable to Java. We want to stay as close to Java and JVM
as possible, mostly in order to re-use the eco-system and libraries
already available.

## Quick Start

Here is a classic "hello, world" example:

```
import org.eolang.printed;
import org.eolang.string;
copy cli(
  copy printed(
    copy string("Hello, world!")
  )
);
```

This code will compile into a `.java` class that will compile into
a `.class` byte code that will run and print "Hello, world!".

What exactly happens here? ...
