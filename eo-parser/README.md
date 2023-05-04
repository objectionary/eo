<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parser.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-parser)
[![Javadoc](http://www.javadoc.io/badge/org.eolang/eo-parser.svg)](http://www.javadoc.io/doc/org.eolang/eo-parser)

# How to Test

This documentation provides a step-by-step guide to test the `EO` code in `eo-parser` package. There are two files provide 
testing `EO` files: `PacksTest` and `TypoTests`.

The `PacksTest` class contains two test methods:

1.  `parsesPacks()` - This method tests if the packs are parsed correctly. It reads YAML files from the `org/eolang/parser/packs/` directory and checks if there are any failures. If there are any failures, the test is aborted.
2.  `createsXaxStoryWithXslStylesheets()` - This method tests if the XSL stylesheets are created correctly. It reads YAML files from the `org/eolang/parser/xax/` directory and creates an XaxStory object. The test passes if the object is created successfully.

`TypoTests` contains only `checksPacks()` method with same functionality as `parsesPacks()`. But it takes YAML files from `org/eolang/parser/typos/` directory.

To create YAML file, you can follow the following steps:

1.  Start by creating a brief introduction to the file and its purpose. In this case, the YAML file is used for defining the XSLT stylesheets and the tests for a program, as well as defining aliases and the main function.
2.  Next, provide a section that explains the different sections of the YAML file. In this case, there are three sections: `xsls`, `tests`, and `eo`.
3.  For the `xsls` section, explain that it is used to define the XSLT stylesheets that will be used for transforming the input program. Provide a brief explanation of each stylesheet listed in the section.
4.  For the `tests` section, explain that it is used for defining the tests that will be run on the program. Provide a brief explanation of each test listed in the section.
5.  For the `eo` section, explain that it is used for defining aliases and the main function for the program. Provide a brief explanation of each alias listed in the section, and how they can be used in the program. Also, explain the purpose of the `main` function and how it works.
6.  Finally, provide a conclusion that summarizes the purpose and contents of the YAML file. You can also include any additional information that might be helpful for users who are working with the file.

Here is an example of what the YAML file might look like:

## YAML File Documentation

Considering the following `yaml` file:

```yaml
xsls:
  - /org/eolang/parser/add-refs.xsl
  - /org/eolang/parser/expand-aliases.xsl
  - /org/eolang/parser/resolve-aliases.xsl
  - /org/eolang/parser/add-default-package.xsl
tests:
  - /program/errors[count(*)=0]
  - /program/objects[count(o)=1]
  - //o[@base='org.eolang.and' and @line='8']
  - //o[@base='foo']
  - //o[@base='$']
  - //o[@base='^']
  - //o[@base='Q']
eo: |
  +alias foo
  +alias scanner org.eolang.txt.scanner
  +alias stdin org.eolang.io.stdin
  +alias stdout org.eolang.io.stdout
  +foo Some other meta

  [args] > main
    and
      (scanner stdin).next-line > line!
      (stdout "You entered" line).print
      $
      ^.i
      foo
      Q.org.eolang.random
```

### Sections

This YAML file is used to define the XSLT stylesheets and tests for a program, as well as defining aliases and the main function.
It consists of three sections: `xsls`, `tests`, and `eo`. The `xsls` section is used to define the XSLT stylesheets, the `tests` section is used for defining the tests, and the `eo` section is used for defining aliases and the main function.

#### xsls

This section is used to define the XSLT stylesheets that will be used for transforming the input program. The following stylesheets are defined in this section:
-   `/org/eolang/parser/add-refs.xsl`: This stylesheet is used to add references to the program.
-   `/org/eolang/parser/expand-aliases.xsl`: This stylesheet is used to expand aliases in the program.
-   `/org/eolang/parser/resolve-aliases.xsl`: This stylesheet is used to resolve aliases in the program.
-   `/org/eolang/parser/add-default-package.xsl`: This stylesheet is used to add a default package to the program.

#### tests

This section is used for defining the tests that will be run on the program. The following tests are defined in this section:
-   `/program/errors[count(*)=0]`: This test checks that there are no errors in the program.
-   `/program/objects[count(o)=1]`: This test checks that there is only one object in the program.
-   `//o[@base='org.eolang.and' and @line='8']`: This test checks that there is an object with the base `org.eolang.and` and the line number `8`.
-   `//o[@base='foo']`: This test checks that there is an object with the base `foo`.
-   `//o[@base='$']`: This test checks that there is an object with the base `$`.
-   `//o[@base='^']`: This test checks that there is an object with the base `^`.
-   `//o[@base='Q']`: This test checks that there is an object with the base `Q`.

#### eo

This section is used for EO code which should be tested.
