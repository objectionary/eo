<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.org.eolang/eo-maven-plugin.svg)](https://maven-badges.herokuapp.com/maven-central/org.org.eolang/eo-maven-plugin)

You can play with EOLANG here, in a few simple steps:

First, clone this repo to your local machine and go
to the `sandbox` directory (you will need
[Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
installed):

```bash
$ git clone https://github.com/cqfn/eo.git
$ cd eo/sandbox
```
Second, compile the transpiler and the runtime of the EO programming language (you will need
[Maven 3.3+](https://maven.apache.org/)
and [Java SDK 8+](https://www.java.com/en/download/) installed):  
```bash
$ mvn -DskipTests clean install
```  
*You need to run the above command only once. This will install the runtime & the transpiler to your machine.*  
Then, compile the code of the sandbox:
```bash
$ mvn clean compile
```  
Intermediary `*.xml` files will be generated in the `target` directory (it will
be created). Also, there will be `*.java` and `*.class` files. Feel free to analyze
them: EO is parsed into XML, then translated to Java, and then compiled
by Java SDK to Java bytecode. Finally, just run the bytecode program through JRE. For example, to run the Fibonacci example, do the following:  

```bash
$ ./run.sh appFibonacci 9
The program has dataized to: 9th Fibonacci number is 34


real	0m0.177s
user	0m0.175s
sys	0m0.037s

```  
The first argument of `./run.sh` is the name of the object to be dataized, while all the other arguments are passed to that object as its free attributes.  

Have a look at our samples in the [Examples](#examples) section.  


You can modify our `*.eo` files or add yours, just run `mvn clean compile` to compile them
again and then do `run.sh` to run them.

Should work. If it doesn't, [submit an issue](https://github.com/cqfn/eo/issues),
we will fix it.

[Have fun!](https://www.elegantobjects.org)

## Examples
- [Examples](#examples)
    - [between](#between)
    - [count](#count)
    - [factorial](#factorial)
    - [factorialTail](#factorialtail)
    - [fibonacciTail](#fibonaccitail)
    - [mergesort](#mergesort)
    - [pi](#pi)
    - [strarrtostdout](#strarrtostdout)
    - [sum](#sum)
    - [unique](#unique)

#### [between](eo/between.eo)
Checks if the first argument is within the next two arguments.  
*Expects integer arguments in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appBetween 1 -5 9
```  

**Result:**  
```
The program has dataized to: 1 is within [-5;9]:yes

real	0m0.177s
user	0m0.179s
sys	0m0.028s
```

#### [count](eo/count.eo)
Count all the occurences of the first argument in the input array (the rest arguments).  
*Expects string array in the stdin*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appCount a a b c d e f g a
```  

**Result:**  
```
The program has dataized to: 2

real	0m0.195s
user	0m0.217s
sys	0m0.034s
```

#### [factorial](eo/factorial.eo)
Evaluates to the factorial of `n` (plain recursion algorithm).  
*Expects integer n in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appFactorial 10
```  

**Result:**  
```
The program has dataized to: 10! = 3628800

real	0m0.155s
user	0m0.167s
sys	0m0.013s

```

#### [factorialTail](eo/factorialTail.eo)
Evaluates to the factorial of `n` (tail recursion algorithm).  
*Expects integer n in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appFactorialTail 10
```  

**Result:**  
```
The program has dataized to: 10! = 3628800

real	0m0.176s
user	0m0.198s
sys	0m0.005s
```

#### [fibonacciTail](eo/fibonacciTail.eo)
Evaluates to the Fibonacci number #n (tail recursion algorithm).  
*Expects integer n in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appFibonacciTail 100
```  

**Result:**  
```
The program has dataized to: 100th Fibonacci number is 3736710778780434371

real	0m0.188s
user	0m0.205s
sys	0m0.046s
```
#### [mergesort](eo/mergesort.eo)
Performs the merge sorting of the input array.  
*Expects integer array in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appMergesort 5 9 4 3 1 0
```  

**Result:**  
```
0 
1 
3 
4 
5 
9 
The program has dataized to: true

real	0m5.741s
user	0m6.557s
sys	0m0.178s
```

#### [pi](eo/pi.eo)
Evaluates to the pi value. The greater `n` is, the more precisely the `pi` is computed.  
*Expects integer n in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appPi 10000
```  

**Result:**  
```
The program has dataized to: Actual value = 3.1415926535; computed value(n=10000) = 3.1416926436

real	0m1.468s
user	0m2.042s
sys	0m0.090s
```

#### [strarrtostdout](eo/strarrtostdout.eo)
Prints string arrays.  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appStrarrtostdout a aa aaa
```  

**Result:**  
```
a 
aa 
aaa 
The program has dataized to: true

real	0m0.176s
user	0m0.174s
sys	0m0.034s
```

#### [sum](eo/sum.eo)
Evaluates to the sum of numbers of the input array.  
*Expects integer arrays in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appSum 10 10 10 10 10 1 1 1 1 1
```  

**Result:**  
```
The program has dataized to: 55

real	0m0.181s
user	0m0.188s
sys	0m0.027s
```

#### [unique](eo/unique.eo)
Evaluates to the array that contains only unique values of the input array.  
[Back to Contents](#examples)

**How to Run**
```
./run.sh appUnique a b a a c d c c c d d e
```  

**Result:**  
```
a 
b 
c 
d 
e 
The program has dataized to: true

real	0m0.236s
user	0m0.335s
sys	0m0.047s
```