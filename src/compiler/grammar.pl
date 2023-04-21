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

% <output> ::= "print" "(" <expression1> ")"
% <expression1> ::= <expression> "," <expression1> | <expression

output --> [print], ['('], expression, [')'].
expression1 --> expression, [,], expression1.
expression1 --> expression.