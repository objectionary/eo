# The EO Programming Language Standard Library
This section covers *The EO Standard Library* which is a collection of utility objects for writing programs in *EO*.
## Command Line Interface Output
*The EO Standard Library* contains two objects for the CLI output: `sprintf` for strings formatting and `stdout` for plain text output. 
### Plain Text Output
For plain text output, the `stdout` object is used.   
**Fully Qualified Name:** `org.eolang.io.stdout`.
#### Usage
The `stdout` object has one free attribute `text` which should be bounded to the text to be printed.  
The object bounded to the `text` attribute must be of `string` type.  
The `stdout` does not put the End of Line character at the end of the output, so the `\n` escape sequence should be used in case if such a behavior is needed.  
For the complete list of escape sequences supported by `stdout`, see the corresponding section of [the article](https://docs.oracle.com/javase/tutorial/java/data/characters.html).  
#### Examples
##### The Plain Old “Hello, World” Example
```
+package sandbox
+alias stdout org.eolang.io.stdout

[args...] > app
  (stdout "Hello, World!\n") > @
```
In this example, we apply (or copy) an abstract `stdout` object by bounding its free attribute `text` to the object `"Hello, World!\n"` of type `string`.  
Next, we bound the resulting copy of the object to the `@` (phi) attribute of the `app` object. 
###### Running
```
IN$: ./run.sh
OUT>: Hello, World!
IN$: 
```

##### Print the First Word of the User's Input
```
+package sandbox
+alias stdout org.eolang.io.stdout

[args...] > app
  stdout > @
    get.
      args
      0
```
In this example, we apply (or copy) an abstract `stdout` object by bounding its free attribute `text` to the attribute `get` of the `args` object, whose free attribute `i` (index) is bounded to the object `0` of type `int`.  
Next, we bound the resulting copy of the object to the `@` (phi) attribute of the `app` object.  
Conceptually, we extract the first word the user types in the command line when running the application and then print the word back. 
###### Running
```
IN$: ./run.sh Hello Bye Thanks Ok
OUT>: HelloIN$: 
```
**Note:** here the `Hello` is printed with no `EOL` character at the end of the line, because of the absence of it in the user input. 

### Formatting String
For strings formatting, the `sprintf` object is used.  
String formatting is the process of data injection into the string, optionally applying format patterns to the data.  
**Fully Qualified Name:** `org.eolang.txt.sprintf`.
#### Usage
The `sprintf` object has two free attributes:  
1. `format` for the format `string` that describes the formatting of the resulting string.
2. `args` for the data being injected into the string. `args` may be empty or may have any number of objects. `args` must be consistent with the `format` (i.e. the number and the types (as well as their order) of objects in the `format` and the `args` should be the same).    
  
If the `sprintf` object is fully applied (i.e. all the attributes are bound), its `@` (phi) attribute contains the resulting formatted object of type 'string'.  
For the `format` syntax reference, see [this article](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html#syntax).  
#### Examples
##### Print 'Em All
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
In this example, we apply (or copy) an abstract `sprintf` object by bounding its free attribute `format` to the object `"int: %d, bool: %b, string: %s\n"` of type `string` and its free attribute `args` to objects `2` of type `int`, `2.less 0` (which is the object `false`) of type `bool`, and `"Hey"` of type `string`.  
Next, we bound the resulting copy of the object to the `formatted_string` identifier.  
Then, we supply the `formatted_string` for its application to `stdout`.  
Finally, we bound the resulting copy of the object `stdout` to the `@` (phi) attribute of the `app` object.  
Conceptually, we format the string and then print it to the terminal.  
###### Running
```
IN$: ./run.sh
OUT>: int: 2, bool: false, string: Hey
IN$: 
```
