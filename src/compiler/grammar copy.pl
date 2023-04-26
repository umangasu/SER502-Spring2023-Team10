:- table block/2.
:- table expression/2.
:- table term/2.
:- table factor/2.

:- table block/3.
:- table expression/3.
:- table term/3.
:- table factor/3.


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

% <declaration> ::= <type> <variable> | <type> <variable> "," <variable1>
declaration --> type, variable.
declaration --> type, variable, [','], variable1.

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
variable1 --> variable, [','], variable1.
variable1 --> variable.

% <assignment> ::= <identifier> "=" <expression> | <identifier> "=" <ternary>
assignment --> identifier, [=], expression.
assignment --> identifier, [=], ternary.

% <identifier> ::= ^[_a-zA-Z][_a-zA-Z0-9]*
% <string> ::= "'" [a-zA-Z0-9_@!.,\s]* "'"
% <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
% <integer> ::= <digit> | <digit> <integer>
% <float> ::= <integer> "." <integer>
% <bool> ::= True | False | "!" <bool> | <condition>
identifier --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
string --> [S], {atom(S)}.
integer --> [N], {integer(N)}.
float --> [F], {float(F)}.
boolean --> [true] | [false].
boolean --> [!], boolean.
boolean --> condition.

% <ternary> ::= <condition> "?" <expression> ":" <expression>
ternary --> condition, [?], expression, [:], expression.

% <expression> ::= <expression> + <expression> | <expression> - <expression>  
% | <expression> * <expression> | <expression> / <expression>
% | (<expression>) | <integer> | <float> | <identifier> | <identifier> "++" 
% | <identifier> "--" | <string> | <booleam>

expression --> expression, [+], term.
expression --> expression, [-], term.
expression --> term.

term --> term, [*], factor.
term --> term, [/], factor.
term --> factor.

factor --> integer.
factor --> float.
factor --> string.
factor --> boolean.

factor --> identifier.
factor --> identifier, [++].
factor --> identifier, [--].

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
for_loop --> [for], ['('], identifier, [=], for_integer, [;], condition, [;], expression, [')'], ['{'], block, ['}'].
for_range --> [for], identifier, [in], [range], ['('], for_integer, [','], for_integer, [')'], ['{'], block, ['}'].
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% program(t_program(program, '{', Block, '}')) --> [program], ['{'], block(Block), ['}'].

% block(t_block(Statement)) --> statement(Statement).
% block(t_block(Block, Statement)) --> block(Block), statement(Statement).

% statement(t_statement(Declaration, ;)) --> declaration(Declaration), [;].
% statement(t_statement(Assignment, ;)) --> assignment(Assignment), [;].
% statement(t_statement(IfStatement)) --> if_statement(IfStatement).
statement(t_statement(WhileLoop)) --> while_loop(WhileLoop).
statement(t_statement(ForLoop)) --> for_loop(ForLoop).
statement(t_statement(ForRange)) --> for_range(ForRange).
% statement(t_statement(PrintStatement, [;])) --> print_statement(PrintStatement), [;].

% declaration(t_declaration(Type, Variable)) --> type(Type), variable(Variable).
% declaration(t_declaration(Type, Variable, ',', Variable1)) --> type(Type), variable(Variable), [','], variable1(Variable1).

% type(t_type(int)) --> [int].
% type(t_type(float)) --> [float].
% type(t_type(char)) --> [char].
% type(t_type(string)) --> [string].
% type(t_type(bool)) --> [bool].

% variable(t_variable(Identifier)) --> identifier(Identifier).
% variable(t_variable(Assignment)) --> assignment(Assignment).

% variable1(t_variable1(Variable, ',', Variable1)) --> variable(Variable), [','], variable1(Variable1).
% variable1(t_variable1(Variable)) --> variable(Variable).

% assignment(t_assignment(Identifier, =, Expression)) --> identifier(Identifier), [=], expression(Expression).
assignment(t_assignment(Identifier, =, Ternary)) --> identifier(Identifier), [=], ternary(Ternary).


% identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
% string(t_string(S)) --> [S], {atom(S)}.
% integer(t_integer(N)) --> [N], {integer(N)}.
% float(t_float(F)) --> [F], {float(F)}.
% boolean(t_boolean(true, false)) --> [true] | [false].
% boolean(t_boolean(!, Boolean)) --> [!], boolean(Boolean).
% boolean(t_boolean(Condition)) --> condition(Condition).

ternary(t_ternary(Condition, Expression1, Expression2)) --> condition(Condition), [?], expression(Expression1), [:], expression(Expression2).

% expression(t_expression(Expression, +, Term)) --> expression(Expression), [+], term(Term).
% expression(t_expression(Expression, -, Term)) --> expression(Expression), [-], term(Term).
% expression(t_expression(Term)) --> term(Term).

% term(t_term(Term, *, Factor)) --> term(Term), [*], factor(Factor).
% term(t_term(Term, /, Factor)) --> term(Term), [/], factor(Factor).
% term(t_term(Factor)) --> factor(Factor).

% factor(t_factor(Integer)) --> integer(Integer).
% factor(t_factor(Float)) --> float(Float).
% factor(t_factor(String)) --> string(String).
% factor(t_factor(Boolean)) --> boolean(Boolean).

% factor(t_factor(Identifier)) --> identifier(Identifier).
factor(t_factor(Identifier, ++)) --> identifier(Identifier), [++].
factor(t_factor(Identifier, --)) --> identifier(Identifier), [--].

factor(t_factor('(', Expression, ')')) --> ['('], expression(Expression), [')'].

% if_statement(t_if_statement(if, '(', Condition, ')', '{', Block, '}', IfStatement1)) --> [if], ['('], condition(Condition), [')'], ['{'], block(Block), ['}'], if_statement1(IfStatement1).
% if_statement(t_if_statement(IfStatement1)) --> if_statement1(IfStatement1).
% if_statement1(t_if_statement1()) --> [].
% if_statement1(t_if_statement1(else, '{', Block, '}')) --> [else], ['{'], block(Block), ['}'].
% if_statement1(t_if_statement1(else, IfStatement)) --> [else], if_statement(IfStatement).

% condition(t_condition(Expression1, RelationOp, Expression2)) --> expression(Expression1), relation_op(RelationOp), expression(Expression2).
% condition(t_condition(Condition1, LogicalOp, Condition1)) --> condition(Condition1), logical_op(LogicalOp), condition(Condition1).

% relation_op(t_relation_op(<)) --> [<].
% relation_op(t_relation_op(<=)) --> [<=].
% relation_op(t_relation_op(>)) --> [>].
% relation_op(t_relation_op(>=)) --> [>=].
% relation_op(t_relation_op(==)) --> [==].
% relation_op(t_relation_op('!=')) --> ['!='].

% logical_op(t_logical_op('&&')) --> ['&&'].
% logical_op(t_logical_op('||')) --> ['||'].

while_loop(t_while_loop(while, '(', Condition, ')', '{', Block, '}')) --> [while], ['('], condition(Condition), [')'], ['{'], block(Block), ['}'].
for_loop(t_for_loop(for, '(', Identifier, =, ForInteger, ;, Condition, ;, Expression, ')', '{', Block, '}')) --> [for], ['('], identifier(Identifier), [=], for_integer(ForInteger), [;], condition(Condition), [;], expression(Expression), [')'], ['{'], block(Block), ['}'].
for_range(t_for_range(for, Identifier, in, range, '(', ForInteger1, ',', ForInteger2, ')', '{', Block, '}')) --> [for], identifier(Identifier), [in], [range], ['('], for_integer(ForInteger1), [','], for_integer(ForInteger2), [')'], ['{'], block(Block), ['}'].
for_integer(t_for_integer(Integer)) --> integer(Integer).
for_integer(t_for_integer(Identifier)) --> identifier(Identifier).

% print_statement(t_print_statement(print, '(', Expression1, ')')) --> [print], ['('], expression1(Expression1), [')'].
% expression1(t_expression1(Expression, [','], Expression1)) --> expression(Expression), [','], expression1(Expression1).
% expression1(t_expression1(Expression)) --> expression(Expression).
