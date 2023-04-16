:- table block/2.

program --> [program], ['{'], block , ['}'].

% <block> ::= <statement> | <statement> <block>
block --> statement.
block --> block, statement.


statement --> [].