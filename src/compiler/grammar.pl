:- table block/2.



program --> [program], ['{'], block , ['}'].

% <block> ::= <statement> | <statement> <block>
block --> statement.
block --> block, statement.


statement --> [].

identifier --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
string --> [S], {atom(S)}.
integer --> [N], {integer(N)}.
float --> [F], {float(F)}.
boolean --> [true] | [false].