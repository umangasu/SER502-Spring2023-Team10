% Author - Sanket Surendra Kapse, Soham Prabhakar Patil, Sambhav Kapoor
% Purpose - Grammar File
% Version - Final
% Date - 29 April 2023

:- table block/2.
:- table expression/2.
:- table term/2.
:- table factor/2.

:- table block/3.
:- table condition/3.

program --> [program], ['{'], ['}'].
program --> [program], ['{'], block , ['}'].

% <block> ::= <statement> | <statement> <block>
block --> statement.
block --> block, statement.

% <statement> ::= <declaration> ";" | <assignment> ";" | <if_statement> | 
% <while_loop> | <for_loop> | <for_range> | <print_statement> ";"
statement --> declaration, [;].
statement --> assignment, [;].
statement --> if_statement.
statement --> while_loop.
statement --> for_loop.
statement --> for_range.
statement --> print_statement, [;].

% <declaration> ::= <type> <variable>
declaration --> type, variable.

% <type> ::= "int" | "string" | "bool"
type --> [int].
type --> [string].
type --> [bool].

% <variable> ::= <identifier> | <assignment>
variable --> identifier.
variable --> assignment.

% <assignment> ::= <identifier> "=" <expression> | <identifier> "=" <ternary>
assignment --> identifier, [=], expression.
assignment --> identifier, [=], string.
assignment --> identifier, [=], ternary.
assignment --> identifier, [++].
assignment --> identifier, [--].

% <identifier> ::= ^[_a-zA-Z][_a-zA-Z0-9]*
% <string> ::= "'" [a-zA-Z0-9_@!.,\s]* "'"
% <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
% <integer> ::= <digit> | <digit> <integer>
% <float> ::= <integer> "." <integer>
% <bool> ::= True | False | <condition>
identifier --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
string --> [S], {atom(S)}.
integer --> [N], {integer(N)}.
boolean --> [true] | [false].

% <ternary> ::= <condition> "?" <expression> ":" <expression>
ternary --> condition, [?], expression, [:], expression.

% <expression> ::= <expression> + <expression> | <expression> - <expression>  
% | <expression> * <expression> | <expression> / <expression>
% | (<expression>) | <integer> | <float> | <identifier> | <identifier> "++" 
% | <identifier> "--" | <string>

expression --> expression, [+], term.
expression --> expression, [-], term.
expression --> term.

term --> term, [*], factor.
term --> term, [/], factor.
term --> factor.

factor --> integer.
factor --> string.

factor --> identifier.

factor --> ['('], expression, [')'].

% <if_statement> ::= "if" "(" <condition> ")" "{" <block> "}" | <if_statement1>
% <if_statement1> ::= ""
% <if_statement1> ::= "else" "{" <block> "}"
% <if_statement1> ::= "else" <if_statement>

if_statement --> [if], ['('], condition, [')'], ['{'], block, ['}'].
if_statement --> if_statement1.
if_statement1 --> [].
if_statement1 --> [else], ['{'], block, ['}'].
if_statement1 --> [else], if_statement.


% <condition> ::= <expression> <relation_op> <expression>
% <condition> ::= <expression> <logical_op> <expression>

condition --> expression, relation_op, expression.
condition --> condition, logical_op, condition.
condition --> boolean.
condition --> [!], condition.


% <relation_op> ::= "<" | "<=" | ">" | ">=" | "==" | "!="
% <logical_op> ::= "&&" | "||"

relation_op --> [<].
relation_op --> [<=].
relation_op --> [>].
relation_op --> [>=].
relation_op --> [==].
relation_op --> ['!='].

logical_op --> ['&&'].
logical_op --> ['||'].


% <while_loop> ::= "while" "(" <condition> ")" "{" <block> "}"
% <for_loop> ::= "for" "(" <identifier> "=" <for_integer> ";" <condition> ";" <expression> ")" "{" <block> "}"
% <for_range> ::= "for" <identifier> "in" "range" "(" <for_integer> "," <for_integer> ")" "{" <block> "}"
% <for_integer> ::= <integer> | <identifier>

while_loop --> [while], ['('], condition, [')'], ['{'], block, ['}'].
for_loop --> [for], ['('], identifier, [=], for_integer, [;], condition, [;], assignment, [')'], ['{'], block, ['}'].
for_range --> [for], identifier, [in], [range], ['('], for_integer, [';'], for_integer, [')'], ['{'], block, ['}'].
for_integer --> integer.
for_integer --> identifier.


% <output> ::= "print" "(" <expression1> ")"
% <expression1> ::= <expression> "," <expression1> | <expression

print_statement --> [print], ['('], print_values, [')'].
print_values --> string, [','], print_values.
print_values --> identifier, [','], print_values.
print_values --> integer, [','], print_values.
print_values --> integer.
print_values --> string.
print_values --> identifier.