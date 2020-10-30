grammar test;

text: foo EOF;
foo: 'X' tail;
tail: (' ' 'X')*;
