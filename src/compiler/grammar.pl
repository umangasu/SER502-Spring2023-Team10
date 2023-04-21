:- table block/2.
:- table expression/2.
:- table term/2.
:- table factor/2.



program --> [program], ['{'], block , ['}'].

% <block> ::= <statement> | <statement> <block>
block --> statement.
block --> block, statement.

% <statement> ::= <declaration> ";" | <assignment> ";" | <if_statement> | 
% <while_loop> | <for_loop> | <for_range> | <print_statement> ";"
statement --> declaration, [;].
statement --> assignment [;].
statement --> if_statement.
statement --> while_loop.
statement --> for_loop.
statement --> for_range.
statement --> print_statement, [;].

% <declaration> ::= <type> <variable> | <type> <variable> "," <variable1>
declaration --> type, variable.
declaration --> type, variable, [,], variable1.

% <type> ::= "int" | "float" | "char" | "string" | "bool"
type --> [int].
type --> [float].
type --> [char].
type --> [string].
type --> [bool].

% <variable> ::= <identifier> | <assignment>
variable --> identifier.
variable --> assignment.

% <variable1> ::= <variable> "," <variable1> | <variable>
variable1 --> variable, ",", variable1.
variable1 --> variable.


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
condition --> expression, logical_op, expression.


% <relation_op> ::= "<" | "<=" | ">" | ">=" | "==" | "!="
% <logical_op> ::= "&&" | "||"

relation_op --> [<].
relation_op --> [<=].
relation_op --> [>].
relation_op --> [>=].
relation_op --> [==].
relation_op --> [!=].

logical_op --> [&&].
logical_op --> [||].


% <while_loop> ::= "while" "(" <condition> ")" "{" <block> "}"
% <for_loop> ::= "for" "(" <identifier> "=" <for_integer> ";" <condition> ";" <expression> ")" "{" <block> "}"
% <for_range> ::= "for" <identifier> "in" "range" "(" <for_integer> "," <for_integer> ")" "{" <block> "}"
% <for_integer> ::= <integer> | <identifier>

while_loop --> [while], ['('], condition, [')'], ['{'], block, ['}'].
for_loop --> [for], ['('], identifier, [=], for_integer, [;], condition, [;], expression, [')'], ['{'], block, ['}'].
for_range --> [for], identifier, [in], [range], ['('], for_integer, [,], for_integer, [')'], ['{'], block, ['}'].
for_integer --> integer.
for_integer --> identifier.


% <output> ::= "print" "(" <expression1> ")"
% <expression1> ::= <expression> "," <expression1> | <expression

output --> [print], ['('], expression, [')'].
expression1 --> expression, [,], expression1.
expression1 --> expression.