## What this directory is for?
This directory proposes and demonstrates an alternative branch of the EO transpiler based on a new transcompilation model of EO programs to Java source codes.  
The old (CQFN) model is depicted [here](https://miro.com/app/board/o9J_lM0FZHk=/).
And the visualization of our model can be found [here](https://miro.com/app/board/o9J_lMMDKrk=/).  
Also you might want to read [a draft of our paper](docs/EO%20Programming%20Language%20Transcompilation%20Model%20for%20Java%20Source%20Code%20Generation.pdf) that describes the model.
## How is this transpiler better?
Our transcompilation model has two great advantages:
1. It if much faster than the CQFN original implementation.
2. The output Java code is much easier to read and comprehend. The runtime library is also much more readable and understandable.

You can see the difference in performance of the old and the new models below:
![This model is faster!](docs/faster.png "This model is faster!") 
Please, refer to [this Google Sheet](https://docs.google.com/spreadsheets/d/1YsalbO6piExC3begifeNNsaz7PEDsqlV3Xx7c6TGbOU/edit?usp=sharing) for more comparisons.

## Table of Contents
- [What this directory is for?](#what-this-directory-is-for)
- [How is this transpiler better?](#how-is-this-transpiler-better)
- [Table of Contents](#table-of-contents)
- [How to Run This Code](#how-to-run-this-code)
- [Examples](#examples)
    - [between](#between)
    - [binarytree](#binarytree)
    - [contains](#contains)
    - [count](#count)
    - [factorial](#factorial)
    - [factorialTail](#factorialtail)
    - [fibonacciTail](#fibonaccitail)
    - [first](#first)
    - [floatarrtostdout](#floatarrtostdout)
    - [floatarrtostrarr](#floatarrtostrarr)
    - [imax](#imax)
    - [imin](#imin)
    - [indexof](#indexof)
    - [last](#last)
    - [max](#max)
    - [min](#min)
    - [merge](#merge)
    - [mergesort](#mergesort)
    - [pi](#pi)
    - [remove](#remove)
    - [selectionsort](#selectionsort)
    - [strarrtostdout](#strarrtostdout)
    - [sum](#sum)
    - [unique](#unique)

## How to Run This Code

You can play with EOLANG here, in a few simple steps:

First, clone this repo to your local machine and go
to the `eo` directory (you will need
[Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
installed):

```bash
$ git clone https://github.com/cqfn/eo.git
$ cd eo
```
Second, compile the transpiler and the runtime of the EO programming language (you will need
[Maven 3.3+](https://maven.apache.org/)
and [Java SDK 8+](https://www.java.com/en/download/) installed):  
```bash
$ mvn clean install
```  
*You need to run the above command only once. This will install the runtime & the transpiler to your machine.*  
Then, compile the code of the sandbox:
```bash
$ cd sandbox/hse
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

## Examples
- [What this directory is for?](#what-this-directory-is-for)
- [How is this transpiler better?](#how-is-this-transpiler-better)
- [Table of Contents](#table-of-contents)
- [How to Run This Code](#how-to-run-this-code)
- [Examples](#examples)
    - [between](#between)
    - [binarytree](#binarytree)
    - [contains](#contains)
    - [count](#count)
    - [factorial](#factorial)
    - [factorialTail](#factorialtail)
    - [fibonacciTail](#fibonaccitail)
    - [first](#first)
    - [floatarrtostdout](#floatarrtostdout)
    - [floatarrtostrarr](#floatarrtostrarr)
    - [imax](#imax)
    - [imin](#imin)
    - [indexof](#indexof)
    - [last](#last)
    - [max](#max)
    - [min](#min)
    - [merge](#merge)
    - [mergesort](#mergesort)
    - [pi](#pi)
    - [remove](#remove)
    - [selectionsort](#selectionsort)
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

#### [binarytree](eo/binarytree.eo)
Constructs this binary tree in RAM:  
```
                |--- nil
      |--- 2 ---|
      |         |--- nil
1 --- |
      |         |--- 4
      |--- 3 ---|
                |--- nil
```  
And then sums its nodes and counts them.  
*Expects no arguments in stdin*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appBinaryTree
```  

**Result:**  
```
The program has dataized to: Sum of nodes: 10
Count of nodes:4


real	0m0.210s
user	0m0.197s
sys	0m0.045s
```

#### [contains](eo/contains.eo)
Checks if the first argument is in the input array (the rest arguments).  
*Expects string array in the stdin*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appContains 1 8 9 0 9 5 1
```  

**Result:**  
```
The program has dataized to: true

real	0m0.245s
user	0m0.305s
sys	0m0.045s
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

#### [first](eo/first.eo)
Returns the first element of the given array[5,2,3] or returns error if array is empty  
 
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appFirst
```  

**Result:**  
```
The program has dataized to: 5

real    0m0.288s
user    0m0.139s
sys     0m0.094s

```

#### [floatarrtostdout](eo/floatarrtostdout.eo)
Prints an array of floating point numbers to the console.  
 
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appFloatArrToStdOut
```  

**Result:**  
```
5.000000 
2.200000 
3.100000 
The program has dataized to: true

real    0m0.184s
user    0m0.139s
sys     0m0.046s

```

#### [floatarrtostrarr](eo/floatarrtostrarr.eo)
Converts an array of floats to a string array.
 
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appFloatArrToStrArr
```  

**Result:**  
```
5.000000 
2.200000 
3.100000 
The program has dataized to: true

real    0m0.168s
user    0m0.133s
sys     0m0.045s

```

#### [imax](eo/imax.eo)
Returns the index of the max element in the array[5,2,3] or error if empty 

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appImax
```  

**Result:**  
```
0
The program has dataized to: 0

real    0m0.207s
user    0m0.144s
sys     0m0.048s

```

#### [imin](eo/imin.eo)
Returns the index of the min element in the array[5,2,3] or error if empty 

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appImin
```  

**Result:**  
```
1
The program has dataized to: 1

real    0m0.210s
user    0m0.156s
sys     0m0.047s

```

#### [indexof](eo/indexof.eo)
Returns the index of the first element in the array[5,2,3] equal to the given one[3], otherwise returns -1. 

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appIndexOf
```  

**Result:**  
```
2
The program has dataized to: 2

real    0m0.217s
user    0m0.144s
sys     0m0.051s

```

#### [last](eo/last.eo)
Returns the last element of the array or error if empty 

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appLast
```  

**Result:**  
```
4
The program has dataized to: 4

real    0m0.677s
user    0m0.178s
sys     0m0.115s

```

#### [max](eo/max.eo)
Returns the max element of the array or error if empty 

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appMax
```  

**Result:**  
```
5
The program has dataized to: 5

real    0m0.478s
user    0m0.173s
sys     0m0.104s

```

#### [min](eo/min.eo)
Returns the min element of the array[5,2,4] or error if empty 

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appMin
```  

**Result:**  
```
2
The program has dataized to: 2

real    0m0.304s
user    0m0.138s
sys     0m0.049s

```

#### [merge](eo/merge.eo)
Performs the merge of to arrays[1,2] and [3,4].  

[Back to Contents](#examples)  

**How to Run**
```
./run.sh appMerge 
```  

**Result:**  
```
1 
2 
3 
4 
The program has dataized to: true

real    0m0.203s
user    0m0.149s
sys     0m0.047s

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

#### [remove](eo/remove.eo)
Removes the first occurence of the first argument from the input array (the rest arguments).  
*Expects string array in the stdin*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appRemove 1 0 5 7 8 9 1 4 1
```  

**Result:**  
```
0 
5 
7 
8 
9 
4 
1
The program has dataized to: true

real	0m0.461s
user	0m0.998s
sys	0m0.055s
```

#### [selectionsort](eo/selectionsort.eo)
Performs the selection sorting of the input array.  
*Expects integer array in the stdin only*  
[Back to Contents](#examples)  

**How to Run**
```
./run.sh appSelectionsort 5 1
```  

**Result:**  
```
1 
5 
The program has dataized to: true

real	0m2.706s
user	0m3.420s
sys	0m0.140s
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