<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

[![Donate via Zerocracy](https://www.0crat.com/contrib-badge/C63314D6Z.svg)](https://www.0crat.com/contrib/C63314D6Z)

[![EO principles respected here](https://www.elegantobjects.org/badge.svg)](https://www.elegantobjects.org)
[![Managed by Zerocracy](https://www.0crat.com/badge/C63314D6Z.svg)](https://www.0crat.com/p/C63314D6Z)
[![DevOps By Rultor.com](http://www.rultor.com/b/cqfn/eo)](http://www.rultor.com/p/cqfn/eo)
[![We recommend IntelliJ IDEA](https://www.elegantobjects.org/intellij-idea.svg)](https://www.jetbrains.com/idea/)

[![Build Status](https://travis-ci.org/cqfn/eo.svg?branch=master)](https://travis-ci.org/cqfn/eo)
[![PDD status](http://www.0pdd.com/svg?name=cqfn/eo)](http://www.0pdd.com/p?name=cqfn/eo)
[![Maintainability](https://api.codeclimate.com/v1/badges/e4f7ed144919f7f0d58c/maintainability)](https://codeclimate.com/github/cqfn/eo/maintainability)
[![Hits-of-Code](https://hitsofcode.com/github/cqfn/eo)](https://hitsofcode.com/view/github/cqfn/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/cqfn/eo/blob/master/LICENSE.txt)
![Lines of code](https://img.shields.io/tokei/lines/github/cqfn/eo)
[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parent.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-parent)

**EO** (stands for [Elegant Objects](http://www.yegor256.com/elegant-objects.html) or
ISO 639-1 code of [Esperanto](https://en.wikipedia.org/wiki/Esperanto))
is an object-oriented programming language. It's still a prototype.
It's the future of OOP. Please [contribute](https://github.com/cqfn/eo#how-to-contribute)!
By the way, we're aware of popular semi-OOP languages and we don't think
they are good enough, including
[Java](https://en.wikipedia.org/wiki/Java_%28programming_language%29),
[Ruby](https://en.wikipedia.org/wiki/Ruby_%28programming_language%29),
[C++](https://en.wikipedia.org/wiki/C%2B%2B),
[Smalltalk](https://en.wikipedia.org/wiki/Smalltalk),
[Python](https://en.wikipedia.org/wiki/Python_%28programming_language%29),
[PHP](https://en.wikipedia.org/wiki/PHP),
[C#](https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29):
all of them have something we don't tolerate.

EO is not planning to become a mainstream language&mdash;this is not what
we want. Our main goal is to prove to ourselves that true object-oriented
programming is practically possible. Not just in books and abstract
examples, but in real code that works. That's why EO is being created&mdash;to
put all that "crazy" pure object-oriented ideas into practice and
see whether they can work. It's an experiment, a prototype, a proof-of-concept.

If you want to contribute, please join our
[Telegram chat](https://t.me/elegantobjects) first.

Our Twitter tag is [#eolang](https://twitter.com/search?q=%23eolang).

These things we **don't tolerate**:

  * static/class methods or attributes ([why?](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html))
  * classes ([why?](http://www.yegor256.com/2016/09/20/oop-without-classes.html))
  * implementation inheritance ([why?](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html))
  * mutability ([why?](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html))
  * NULL ([why?](http://www.yegor256.com/2014/05/13/why-null-is-bad.html))
  * global variables/procedures
  * reflection
  * type casting ([why?](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html))
  * scalar types and data primitives
  * annotations ([why?](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html))
  * unchecked exceptions ([why?](http://www.yegor256.com/2015/07/28/checked-vs-unchecked-exceptions.html))
  * operators
  * flow control statements (`for`, `while`, `if`, etc)
  * DSL and [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) ([why?](https://github.com/cqfn/eo/issues/51))

We want EO to be compilable to Java. We want to stay as close to Java and JVM
as possible, mostly in order to re-use the eco-system and libraries
already available.

We also want to have an ability to compile it to any other language, like
Python, C/C++, Ruby, C#, etc. In other words, EO must be platform
independent.

## Quick Start

Here is a simple program that gets a year from command line and tells you
whether it's leap or not:

```
+alias stdout org.eolang.io.stdout
+alias stdin org.eolang.io.stdin
+alias scanner org.eolang.txt.scanner

[args...] > main
  [y] > leap
    or. > @
      and.
        eq. (mod. y 4) 0
        not. (eq. (mod. y 100) 0)
      eq. (mod. y 400) 0
  stdout > @
    sprintf
      "%d is %sa leap year!"
      (args.get 0).nextInt > year!
      if (leap year:y) "" "not "
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
        <version>0.1.10</version>
        <executions>
          <execution>
            <goals>
              <goal>parse</goal>
              <goal>optimize</goal>
              <goal>compile</goal>
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
          <mainClass>org.eolang.phi.Main</mainClass>
          <arguments>
            <argument>main</argument>
            <argument>2008</argument>
          </arguments>
        </configuration>
      </plugin>
    </plugins>
  </build>
  <dependencies>
    <dependency>
      <groupId>org.eolang</groupId>
      <artifactId>eo-runtime</artifactId>
      <version>0.1.10</version>
    </dependency>
  </dependencies>
</project>
```

Then, you just run `mvn clean test` (you will need [Maven 3.3+](https://maven.apache.org/))
and the `.eo` file will be parsed to `.xml` files, transformed to `.java` files,
and then compiled to `.class` files. You can see them all in the `target` directory.
You will need Java 8+.

More examples are [here](https://github.com/cqfn/eo/tree/master/eo-maven-plugin/src/it).

## Tutorial

Let's start with a simple EO program:

```
+alias stdout org.eolang.io.stdout

[] > app
  stdout > @
    "Hello, world!"
```

Here we create a new [abstract object](https://www.yegor256.com/2020/12/01/abstract-objects.html)
named `app`, which has got a single attribute named `@`. The object attached to the attribute
`@` is a copy of the object `stdout` with a single argument `"Hello, world!"`. The object
`stdout` is also [abstract](https://www.yegor256.com/2020/12/01/abstract-objects.html).
It can't be used directly, a copy of it has to be created, with a few requirement arguments provided.
This is how a copy of the object `stdout` is made:

```
stdout
  "Hello, world!"
```

The indentation in EO is important, just like in Python. There have to be two spaces
in front of the line in order to go to the deeper level of nesting. This code can also be written
in a "horizontal" notation:

```
stdout "Hello, world!"
```

Moreover, it's possible to use brackets in order to group arguments and avoid
ambiguity. For example, instead of using a plain string `"Hello, world!"`
we may want to create a copy of the object `stdout` with a more complex
argument: a copy of the object `sprintf`:

```
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[] > app
  stdout > @
    sprintf
      "Hello, %s!"
      "Jeffrey"
```

Here, the object `sprintf` is also [abstract](https://www.yegor256.com/2020/12/01/abstract-objects.html).
It is being copied with two arguments: `"Hello, %s!"` and `"Jeffrey"`. This program
can be written using horizontal notation:

```
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[] > app
  (stdout (sprintf "Hello, %s!" "Jeffrey")) > @
```

The special attribute `@` denotes an object that is being
[decorated](https://www.yegor256.com/2015/02/26/composable-decorators.html).
In this example, the object `app` decorates the copy of the
object `stdout` and through this starts to behave like
the object `stdout`: all attributes of `stdout` become the
attributes of the `app`. The object `app` may have its own
attributes. For example, it's possible to define a new abstract object
inside `app` and use it to build the output string:

```
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[] > app
  stdout (msg "Jeffrey") > @
  [name] > msg
    sprintf "Hello, %s!" name > @
```

Now, the object `app` has two "bound" attributes: `@` and `msg`. The attribute
`msg` has an abstract object attached to it, with a single "free" attribute
`name`.

This is how you iterate:

```
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[] > app
  memory > x
  and. > @
    x.write 2
    while.
      x.less 6
      [i]
        seq > @
          stdout
            sprintf "%dx%d = %d" x x (x.pow 2)
          x.write (x.add 1)
```

This code will print this:

```
2 x 2 = 4
3 x 3 = 9
4 x 4 = 16
5 x 5 = 25
```

Got the idea?  

## The EO Standard Object Collection
This section covers *The EO Standard Object Collection* which is a library of utility objects for writing programs in *EO*.  
### Data Type Objects  
*The EO Programming Language* and *The EO Standard Object Collection* defines these data type objects: `bool`, `int`, `float`, `string`, `char`. 
#### `bool` Data Type Object
The `bool` data type object represents a boolean value (either `true` or `false`) that can be used for performing logical operations.  
**Fully Qualified Name:** `org.eolang.bool` (no aliasing or FQN reference required since the object is automatically imported).  
##### Syntax
The `bool` data type object may be parsed by the EO compiler directly from the source code. The syntax rules for `bool` values are as follows.  
**EBNF Notation**  
```EBNF
BOOL     ::= 'true'
           | 'false'
```
**Railroad Diagram**  
![The Bool Data Type Railroad Diagram](docs/img/BOOL.png "The Bool Data Type Railroad Diagram")  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%b\n%b\n"
      true
      false

```
**Running**  
```
IN$: ./run.sh
OUT>: true
OUT>: false
IN$: 
```
##### `if` Attribute
The `if` attribute object is used for value substitution based on a condition that can be evaluated as a `bool` object.  
The `if` attribute object has two free attributes:  
1. `t` for the substitution if the base `bool` object is `true`.  
2. `f` for the substitution if the base `bool` object is `false`.  
  
If the `if` attribute object is fully applied, it represents the corresponding substitution value.   
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%s\n%s\n%s\nThe max(2, 5) is: %d\n"
      true.if
        "the first value is true"
        "the first value is false"
      false.if
        "the second value is true"
        "the second value is false"
      if.
        2.less 3
        "2 is less than 3"
        "2 is not less than 3"
      (5.less 2).if
        2
        5

```
**Running**  
```
IN$: ./run.sh
OUT>: the first value is true
OUT>: the second value is false
OUT>: 2 is less than 3
OUT>: The max(2, 5) is: 5
IN$: 
```
##### `not` Attribute
The `not` attribute object represents a `bool` object with the inversed inner value of its base `bool` object.  
The `not` attribute object has no free attributes.  
###### Example
In this example, all the answers from the previous example (the `if` attribute section) are inversed with the `not` attribute.  
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "[NOT Edition (all the answers are inversed with .not)]\n%s\n%s\n%s\nThe max(2, 5) is: %d\n"
      true.not.if
        "the first value is true"
        "the first value is false"
      false.not.if
        "the second value is true"
        "the second value is false"
      if.
        (2.less 3).not
        "2 is less than 3"
        "2 is not less than 3"
      (5.less 2).not.if
        2
        5

```
**Running**  
```
IN$: ./run.sh
OUT>: [NOT Edition (all the answers are inversed with .not)]
OUT>: the first value is false
OUT>: the second value is true
OUT>: 2 is not less than 3
OUT>: The max(2, 5) is: 2
IN$: 
```
##### `and` Attribute
The `and` attribute object represents logical conjunction on a variety of `bool` objects.  
The `and` attribute object has one free attribute `x` for the `bool` objects (conjuncts). `x` may be empty or may have any number of `bool` objects.  
  
If the `and` attribute object is applied, it represents the conjunction of the base `bool` object and all the objects bound to the `x` attribute.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  true > a
  true > b
  true > c
  false > d
  stdout > @
    sprintf
      "a && b = %b\na && b && c = %b\na && b && c && d = %b\n"
      a.and b
      a.and b c
      and.
        a
        b
        c
        d

```
**Running**  
```
IN$: ./run.sh
OUT>: a && b = true
OUT>: a && b && c = true
OUT>: a && b && c && d = false
IN$: 
```
##### `or` Attribute
The `or` attribute object represents logical disjunction on a variety of `bool` objects.  
The `or` attribute object has one free attribute `x` for the `bool` objects (disjuncts). `x` may be empty or may have any number of `bool` objects.  
  
If the `or` attribute object is applied, it represents the disjunction of the base `bool` object and all the objects bound to the `x` attribute.    
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  false > a
  false > b
  false > c
  true > d
  stdout > @
    sprintf
      "a || b = %b\na || b || c = %b\na || b || c || d = %b\n"
      a.or b
      a.or b c
      or.
        a
        b
        c
        d

```
**Running**  
```
IN$: ./run.sh
OUT>: a || b = false
OUT>: a || b || c = false
OUT>: a || b || c || d = true
IN$: 
```
##### `while` Attribute
`TODO`
#### `float` Data Type Object
The `float` data type object represents a double-precision 64-bit IEEE 754 floating-point number and can be used to perform various `FPU` computations.  
**Fully Qualified Name:** `org.eolang.float` (no aliasing or FQN reference required since the object is automatically imported).  
##### Syntax
The `float` data type object may be parsed by the EO compiler directly from the source code. The syntax rules for values are as follows.  
**EBNF Notation**  
```EBNF
FLOAT    ::= ( '+' | '-' )? [0-9]+ '.' [0-9]+
```
**Railroad Diagram**  
![The Float Data Type Railroad Diagram](docs/img/FLOAT.png "The Float Data Type Railroad Diagram")  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%f\n%f\n"
      1.5
      -3.71

```
**Running**  
```
IN$: ./run.sh
OUT>: 1.500000
OUT>: -3.710000
IN$: 
```
##### `eq` Attribute
The `eq` attribute object is used for testing if two `float` objects are equal.    
The `eq` attribute object has one free attribute `x` of type `float` that is the second object (the first object is the base object of the `eq` attribute).    
If the `eq` attribute object is applied, it represents the result of the equality test (either `true` (if the objects are equal) or `false` (otherwise)).  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%b\n%b\n"
      1.5.eq 1.5
      -3.71.eq 3.71

```
**Running**  
```
IN$: ./run.sh
OUT>: true
OUT>: false
IN$: 
```
#### `string` Data Type Object
The `string` data type object represents a string literal.  
**Fully Qualified Name:** `org.eolang.string` (no aliasing or FQN reference required since the object is automatically imported).  
##### Syntax
The `string` data type object may be parsed by the EO compiler directly from the source code. The syntax rules for values are as follows.  
**EBNF Notation**  
```EBNF
STRING   ::= '"' ( '\"' | [^"] )* '"'
```
**Railroad Diagram**  
![The String Data Type Railroad Diagram](docs/img/STRING.png "The String Data Type Railroad Diagram")  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%s%s%s"
      "Hello, "
      "World! Welcome to The \"EO Docs\"!"
      "\n"

```
**Running**  
```
IN$: ./run.sh
OUT>: Hello, World! Welcome to The "EO Docs"!
IN$: 
```
##### `eq` Attribute
The `eq` attribute object is used for testing if two `string` objects are equal.    
The `eq` attribute object has one free attribute `x` of type `string` that is the second object (the first object is the base object of the `eq` attribute).    
If the `eq` attribute object is fully applied, it represents the result of the equality test (either `true` (if the objects are equal) or `false` (otherwise)).  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%b\n%b\n%b\n"
      "".eq ""
      "Hey".eq "Hey"
      "Hey".eq "hey"

```
**Running**  
```
IN$: ./run.sh
OUT>: true
OUT>: true
OUT>: false
IN$: 
```
##### `trim` Attribute
The `trim` attribute object is used for trimming the base `string` object (i.e. `trim` is a `string` with whitespace removed from both ends of the base `string`).    
The `trim` attribute object has no free attributes.  
If the `trim` attribute object is applied (called), it represents the resulting trimmed `string`.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%s%s%s"
      "  Hello There  ".trim
      "            !           ".trim
      "\n".trim

```
**Running**  
```
IN$: ./run.sh
OUT>: Hello There!IN$: 
```
Here, the `\n` escape sequence is trimmed as it is a whitespace character. 
##### `toInt` Attribute
The `toInt` attribute object is used for parsing the base `string` object as an `int` object.  
The format of the base `string` object must be as described below:  
1. The first character of the `string` literal may be either `+` or `-`. This indicates the sign of the `int` value. The sign may be omitted (in such a case, the number is positive).  
2. All the other characters of the `string` literal must be decimal digits (`0-9`). 
  
If the format of the base `string` object is incorrect, the `toInt` attribute will fail on its application.  
The `toInt` attribute object has no free attributes.  
If the `toInt` attribute object is applied (called), it represents the parsed `int` object.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n%d\n%d\n"
      "1700".toInt
      "-1500".toInt
      "8".toInt
      "-0".toInt

```
**Running**  
```
IN$: ./run.sh
OUT>: 1700
OUT>: -1500
OUT>: 8
OUT>: 0
IN$: 
```
#### `int` Data Type Object
The `int` data type object represents a 64-bit integer number.  
**Fully Qualified Name:** `org.eolang.int` (no aliasing or FQN reference required since the object is automatically imported).  
##### Syntax
The `int` data type object may be parsed by the EO compiler directly from the source code. The syntax rules for values are as follows.  
**EBNF Notation**  
```EBNF
INT      ::= ( '+' | '-' )? [0-9]+
```
There is also an alternative syntax for hexadecimal numerals (i.e., with the base `16`). This notation implies only non-negative values.
```EBNF
HEX      ::= '0x' [0-9a-f]+
```
**Railroad Diagram**  
![The Int Data Type Railroad Diagram](docs/img/INT.png "The Int Data Type Railroad Diagram")  
And an alternative notation for HEX integers:  
![The Int Data Type Railroad Diagram (HEX Notation)](docs/img/HEX.png "The Int Data Type Railroad Diagram (HEX Notation)")  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n%d\n%#01x\n"
      -157
      1009283
      0xf.add 1
      0xa

```
**Running**  
```
IN$: ./run.sh
OUT>: -157
OUT>: 1009283
OUT>: 16
OUT>: 0xa
IN$: 
```
##### `eq` Attribute
The `eq` attribute object is used for testing if two `int` objects are equal.    
The `eq` attribute object has one free attribute `x` of type `int` that is the second object (the first object is the base object of the `eq` attribute).    
If the `eq` attribute object is fully applied, it represents the result of the equality testing (either `true` (if the objects are equal) or `false` (otherwise)).  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%b\n%b\n"
      eq.
        0xf
        15
      15.eq (0xf.add 1)

```
**Running**  
```
IN$: ./run.sh
OUT>: true
OUT>: false
IN$: 
```
##### `less` Attribute
The `less` attribute object is used for testing if its base `int` object is less than its `x` free attribute (i.e. `$ < x`).  
If the `less` attribute object is fully applied, it represents the result of the testing (either `true` (if the base object is less than `x` free attribute of the `less`) or `false` (otherwise)).  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%b\n%b\n"
      -7.less 0
      less.
        0
        0

```
**Running**  
```
IN$: ./run.sh
OUT>: true
OUT>: false
IN$: 
```
##### `add` Attribute
The `add` attribute object is used to calculate the sum of its base `int` object and the free attribute `x` of type `int` (i.e. `$+x`).  
If the `add` attribute object is fully applied, it represents the resulting sum of the integer numbers.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n"
      add.
        0x10
        16
      -16.add 0x10

```
**Running**  
```
IN$: ./run.sh
OUT>: 32
OUT>: 0
IN$: 
```
##### `sub` Attribute
The `sub` attribute object is used to calculate the difference between its base `int` object and the free attribute `x` of type `int` (i.e. `$-x`).  
If the `sub` attribute object is fully applied, it represents the resulting difference of the integer numbers.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n"
      sub.
        0x10
        16
      -16.sub 0x10

```
**Running**  
```
IN$: ./run.sh
OUT>: 0
OUT>: -32
IN$: 
```
##### `neg` Attribute
The `neg` attribute object is used to negate its base `int` object (i.e. `-$`).  
If the `neg` attribute object is applied (called), it represents the resulting negation of the base `int` object.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n%d\n%d\n"
      5.neg
      0x10.neg
      (17.add 3).neg
      17.neg.add 3

```
**Running**  
```
IN$: ./run.sh
OUT>: -5
OUT>: -16
OUT>: -20
OUT>: -14
IN$: 
```
##### `mul` Attribute
The `mul` attribute object is used to calculate the product of its base `int` object and the free attribute `x` of type `int` (i.e. `$ × x`).  
If the `mul` attribute object is fully applied, it represents the resulting product of the integer numbers.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n%d\n%d\n%d\n"
      -7.mul 0
      13.mul 1
      mul.
        0x10
        0x10
      ((10.mul 10).mul 10).mul 10
      10.mul 10.mul 10.mul 10

```
**Running**  
```
IN$: ./run.sh
OUT>: 0
OUT>: 13
OUT>: 256
OUT>: 10000
OUT>: 10000
IN$: 
```
##### `div` Attribute
`TODO` (does not work properly at the moment)
##### `mod` Attribute
The `mod` attribute object is used to calculate the floor remainder of the integer division of its base `int` object by the `x` free attribute (i.e. `$ fmod x`).  
If the `mod` attribute object is fully applied, it represents the resulting floor modulus (remainder).  
The modulus for `x = 0` is undefined.
The resulting floor modulus has the same sign as the divisor `x`.  
The relationship between the `mod` and `div` operations is as follows:  
`(x div y) * y + x mod y == x`
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n%d\n%d\n%d\n%d\n"
      2.mod 1
      7.mod 5
      113.mod 10
      113.mod -10
      -113.mod 10
      -113.mod -10

```
**Running**  
```
IN$: ./run.sh
OUT>: 0
OUT>: 2
OUT>: 3
OUT>: -7
OUT>: 7
OUT>: -3
IN$: 
```
##### `pow` Attribute
The `pow` attribute object is used to calculate the power of its base `int` object and the free attribute `x` of type `int` (i.e. `$^x`).  
If the `pow` attribute object is fully applied, it represents the resulting power of the base `int` object raised to the power of the `x` attribute.  
###### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%d\n%d\n%d\n%d\n%d\n"
      2.pow 10
      -2.pow 3
      2.pow -10
      2.pow 0
      2.pow 1

```
**Running**  
```
IN$: ./run.sh
OUT>: 1024
OUT>: -8
OUT>: 0
OUT>: 1
OUT>: 2
IN$: 
```
Here, `2^(-10)` results in `0` as well as raising all the integer numbers (except `0`) to the negative power (`-1, -2, -3, ...`).  
#### `char` Data Type Object
The `char` data type object represents a single character.  
**The `char` object is not implemented yet, hence the `char` cannot be used for now.**
**Fully Qualified Name:** `org.eolang.char` (no aliasing or FQN reference required since the object is automatically imported).  
##### Syntax
The `char` data type object may be parsed by the EO compiler directly from the source code. The syntax rules for values are as follows.  
**EBNF Notation**  
```EBNF
CHAR     ::= "'" [0-9a-zA-Z] "'"
```
**Railroad Diagram**  
![The Char Data Type Railroad Diagram](docs/img/CHAR.png "The Char Data Type Railroad Diagram")  
### Command Line Interface Output
*The EO Standard Object Collection* contains two objects for the CLI output: `sprintf` for strings formatting and `stdout` for plain text output. 
#### Plain Text Output. `stdout`
For plain text output, the `stdout` object is used.   
**Fully Qualified Name:** `org.eolang.io.stdout`.
##### Usage
The `stdout` object has one free attribute `text` that should be bound to the text to print.  
The object bound to the `text` attribute must be of `string` type.  
The `stdout` does not put the End of Line character at the end of the output, so the `\n` escape sequence should be used in case if such a behavior is needed.  
For the complete list of escape sequences supported by `stdout`, see the corresponding section of [the article](https://docs.oracle.com/javase/tutorial/java/data/characters.html).  
##### Example 1. The Plain Old “Hello, World”
```
+package sandbox
+alias stdout org.eolang.io.stdout

[args...] > app
  (stdout "Hello, World!\n") > @
``` 
###### Running
```
IN$: ./run.sh
OUT>: Hello, World!
IN$: 
```

##### Example 2. Print the First Word of the User's Input
```
+package sandbox
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    get.
      args
      0
```
###### Running
```
IN$: ./run.sh Hello Bye Thanks Ok
OUT>: HelloIN$: 
```
**Note:** here, the `Hello` is printed with no `EOL` character at the end of the line because of the absence of it in the user input. 

#### Formatting String. `sprintf`
For strings formatting, the `sprintf` object is used.  
String formatting is the process of data injection into the string, optionally applying format patterns to the data.  
**Fully Qualified Name:** `org.eolang.txt.sprintf`.
##### Usage
The `sprintf` object has two free attributes:  
1. `format` for the format `string` that describes the formatting of the resulting `string`.
2. `args` for the data being injected into the string. `args` may be empty or may have any number of objects. `args` must be consistent with the `format` (i.e., the number and the types (as well as their order) of the objects in the `format` and the `args` should be the same).    
  
If the `sprintf` object is fully applied, it represents the resulting formatted `string`.  
For the `format` syntax reference, see [this article](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html#syntax).  
##### Example. Format 'Em All
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  sprintf > formatted_string
    "int: %d, bool: %b, string: %s\n"
    2
    (2.less 0)
    "Hey"

  (stdout formatted_string) > @

```  
###### Running
```
IN$: ./run.sh
OUT>: int: 2, bool: false, string: Hey
IN$: 
```
### Random Number Generation. `random`
*The EO Standard Object Collection* contains the `random` object for generating a cryptographically strong random number.  
**Fully Qualified Name:** `org.eolang.random` (no aliasing or FQN reference required since the object is automatically imported).  
#### Usage
The `random` object has no free attributes. When applied, the `random` object represents the generated random number that is immutable (i.e. cannot be changed). So, every time the new random number is needed, the new application (initialization) of the `random` object is needed.     
The resulting random numberrepresented by the `random` object is of type `float`.  
The value is in the range `0.0` (inclusive) to `1.0` (exclusive).  
#### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  sprintf > formatted_string
    "the 1st random: %f\nthe 2nd random: %f\nthe 3rd random:%f\n"
    random
    random
    random

  (stdout formatted_string) > @

```  
##### Running
```
IN$: ./run.sh
OUT>: the 1st random: 0.125293
OUT>: the 2nd random: 0.074904
OUT>: the 3rd random:0.958538
IN$: 
```
### Arrays
*The EO Standard Object Collection* contains the `array` object for working with arrays of objects.  
**Fully Qualified Name:** `org.eolang.array` (no aliasing or FQN reference required since the object is automatically imported).  
#### `get` Attribute
The `get` attribute object is used to retrieve an object stored at the position `i` of the base `array` object.  
The position `i` must be within 0 and the length of the `array` inclusively.  
When applied, the `get` attribute object represents the object stored at the position `i` of the base `array` object.
##### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    sprintf
      "%s\n%s\n"
      args.get 0
      args.get 1

``` 
In this example the `args` array is used that consists of the CLI parameters passed to the program.
###### Running
```
IN$: ./run.sh Hello, World!
OUT>: Hello,
OUT>: World!
IN$: 
```
#### `append` Attribute
The `append` attribute object is used to append the `x` object at the end of the base `array` object.   
When applied, the `append` attribute object represents the resulting `array` object with the `x` at the end of it. 
##### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  args.append "New Element!" > argsExtended
  stdout > @
    sprintf
      "%s\n%s\n%s\n"
      argsExtended.get 0
      argsExtended.get 1
      argsExtended.get 2

``` 
In this example the `args` array is used that consists of the CLI parameters passed to the program.
###### Running
```
IN$: ./run.sh Hello, World!
OUT>: Hello,
OUT>: World!
OUT>: New Element!
IN$: 
```
#### `reduce` Attribute
The `reduce` attribute object is used to perform the reduction operation of its base `array` object. Reduction is a proccess of accumulating a set of objects into one aggregated object.  
The `reduce` attribute object has two free attributes:  
1. `a` for the initial value of the accumulator.  
2. `f` for the object that represents the reduction function. It must have two free attibutes:  
   1. The first attribute is the current value of the accumulator.  
   2. The second attribute is the current object of the `array`.

The `f` attribute object aggregates the objects of the `array` in the `accumulator`. Objects of the `array` arrive into the `f` in the order these objects are stored in the `array`.      
When applied, the `reduce` attribute object represents the resulting reduced accumulator object. 
##### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  [accumulator current] > reduceFunction
    add. > @
      accumulator
      current.toInt

  reduce. > sum
    args
    0
    reduceFunction

  stdout > @
    sprintf
      "%d\n"
      sum

``` 
In this example the `args` array is used that consists of the CLI parameters passed to the program. The array of numbers passed into the program is reduced into the sum of its elements. 
###### Running
```
IN$: ./run.sh 1 2 3 4 5
OUT>: 15
IN$: 
```
#### `map` Attribute
`TODO` The `map` implementation is not correct.
#### `mapi` Attribute
`TODO` The `map` implementation is not correct.
### Sequencing Computations. `seq`
*The EO Standard Object Collection* contains the `seq` object for sequencing computations.  
The `seq` object has one free attribute `steps` that may have arbitrary number of steps that will be evaluated one by one, from the beginning to the end in the sequential order.  
The `seq` object starts the *datarization* process for each of the objects bound to the `steps` attribute of it.  
On datarization, the `seq` object evaluates into the `bool` object `true`. 
**Fully Qualified Name:** `org.eolang.seq` (no aliasing or FQN reference required since the object is automatically imported).  
#### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  seq > @
    stdout "Hello\n"
    stdout "These objects\n"
    stdout "will be datarized\n"
    stdout "one by one, in sequatial order\n"

``` 
##### Running
```
IN$: ./run.sh
OUT>: Hello
OUT>: These objects
OUT>: will be datarized
OUT>: one by one, in sequatial order
IN$: 
```
### Mutable Storage in Memory. `memory`
*The EO Standard Object Collection* contains the `memory` object for mutable storage in RAM.  
**Fully Qualified Name:** `org.eolang.memory` (no aliasing or FQN reference required since the object is automatically imported).  
**Usage**
To use the `memory` object, the following steps are needed:  
1. Make a copy of the `memory` object and bound it to some attribute.  
2. To put an object into the `memory` object, the `write` attribute object is used. It has the `x` free attribute that is the object to put into the `memory`. The `write` attribute evaluates to `true` on datarization.  
3. To retrieve the object stored in the `memory`, datarization of the `memory` object is used.  
##### Example
```
+package sandbox
+alias sprintf org.eolang.txt.sprintf
+alias stdout org.eolang.io.stdout

[args...] > app
  memory > m
  seq > @
    m.write 1
    m.write (m.add 1)
    m.write (m.add 1)
    m.write (m.add 1)
    stdout (sprintf "%d\n" m)

``` 
###### Running
```
IN$: ./run.sh
OUT>: 4
IN$: 
```
## How it Works?
The entire process of turning an `.eo` program into an executable
binary code constists of a few steps, which must be done
one after another:

  * **Parsing**.
    It's done by the `org.eolang.parser.Syntax` class in the `eo-parser` module. It takes
    the source code in a plain text format and parses into XML document,
    using [ANTLR4](https://www.antlr.org/) and [Xembly](https://www.xembly.org).
    The output of the parser you can find in the `target/eo/parse` directory.

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
    The class `org.eolang.maven.CompileMojo` in the `eo-maven-plugin` module is responsible
    for putting parsing and optimization steps together and then transforming
    the XML document into a collection of `.java` files. There are a number
    of transformations that do this, they all exist in `.xsl` files.
    The output of this step you can find in the `target/generated-sources` directory.

There is also a module called `eo-runtime`, which includes both `.eo` and `.java` code
for most popular and important objects that any of you will need in order
to write even a simple EO program. There are objects like `string`, `int`, `sprintf`,
`stdout`, and so on. By the way, you may want to contribute there by creating new objects.

## How to Contribute

Fork repository, make changes, send us a pull request.
We will review your changes and apply them to the `master` branch shortly,
provided they don't violate our quality standards. To avoid frustration,
before sending us your pull request please run full Maven build:

```bash
$ mvn clean install -Pqulice
```

You will need Maven 3.3+ and Java 8+.

