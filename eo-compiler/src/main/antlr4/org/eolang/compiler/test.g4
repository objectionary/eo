grammar test;

text: foo EOF;

foo:
    'X'
    |
    foo
    '!'
    |
    foo
    '?'
    |
    foo
    tail
    ;

tail: (' ' foo)+;
