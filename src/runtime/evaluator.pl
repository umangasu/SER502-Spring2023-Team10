:- table block/3.

program(t_program(program, '{', Block, '}')) --> [program], ['{'], block(Block), ['}'].
program(t_program(program, '{', '}')) --> [program], ['{'], ['}'].


block(t_block(Statement)) --> statement(Statement).
block(t_block(Block, Statement)) --> block(Block), statement(Statement).

statement(t_statement(Declaration, ;)) --> declaration(Declaration), [;].
statement(t_statement(Assignment, ;)) --> assignment(Assignment), [;].
statement(t_statement(PrintStatement, [;])) --> print_statement(PrintStatement), [;].

declaration(t_declaration(Type, Variable)) --> type(Type), variable(Variable).
declaration(t_declaration(Type, Variable, ',', Variable1)) --> type(Type), variable(Variable), [','], variable1(Variable1).

type(t_type(int)) --> [int].
type(t_type(float)) --> [float].
type(t_type(char)) --> [char].
type(t_type(string)) --> [string].
type(t_type(bool)) --> [bool].

variable(t_variable(Identifier)) --> identifier(Identifier).
variable(t_variable(Assignment)) --> assignment(Assignment).

variable1(t_variable1(Variable, ',', Variable1)) --> variable(Variable), [','], variable1(Variable1).
variable1(t_variable1(Variable)) --> variable(Variable).

assignment(t_assignment(Identifier, =, Expression)) --> identifier(Identifier), [=], expression(Expression).


identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
string(t_string(S)) --> ['"'], [S], ['"'], {atom(S)}.
integer(t_integer(N)) --> [N], {integer(N)}.
float(t_float(F)) --> [F], {float(F)}.
boolean(t_boolean(true, false)) --> [true] | [false].
boolean(t_boolean(!, Boolean)) --> [!], boolean(Boolean).


expression(t_expression(Expression, +, Term)) --> expression(Expression), [+], term(Term).
expression(t_expression(Term)) --> term(Term).

term(t_term(Factor)) --> factor(Factor).
factor(t_factor(Integer)) --> integer(Integer).
factor(t_factor(Float)) --> float(Float).
factor(t_factor(String)) --> string(String).
factor(t_factor(Boolean)) --> boolean(Boolean).

print_statement(t_print_statement(print, '(', PrintValues, ')')) --> [print], ['('], print_values(PrintValues), [')'].
print_values(t_print_values(String, ',', PrintValues)) --> string(String), [','], print_values(PrintValues).
print_values(t_print_values(Identifier, ',', PrintValues)) --> identifier(Identifier), [','], print_values(PrintValues).
print_values(t_print_values(Integer, ',', PrintValues)) --> integer(Integer), [','], print_values(PrintValues).
print_values(t_print_values(Integer)) --> integer(Integer).
print_values(t_print_values(String)) --> string(String).
print_values(t_print_values(Identifier)) --> identifier(Identifier).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Evaluator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Environment Operations code used from Assignment 3 of Sanket Kapse %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Environment Operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup(V, [(_DataType, V, Val) | _], Val). 
lookup(V, [(_DataType, V1, _) | T], Val) :-
    V1 \= V,
    lookup(V, T, Val).
lookup(V, [], _) :- 
    format("variable ~w do not found", [V]),
    fail.


update(DataType, V, NewVal, [], [(DataType, V, NewVal)]).
update(DataType, V, NewVal, [(DataType, V, _) | TEnv], [(DataType, V, NewVal) | TEnv]).
update(DataType, V, NewVal, [HEnv | TEnv], [HEnv | TNewEnv]) :-
    (DataType, V, _) \= HEnv,
    update(DataType, V, NewVal, TEnv, TNewEnv).

check_in_env(V, [(V, _) | _]). 
check_in_env(V, [(V1, _) | T]) :-
    V1 \= V,
    check_in_env(V, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Semantics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program_eval(t_program(program, '{', Block, '}'), XDataType, X, YDataType, Y, Z) :-
    update(XDataType, x, X, [], Env),
    update(YDataType, y, Y, Env, NEnv),
    eval_block(Block, NEnv, NewEnv),
    lookup(z, NewEnv, Z).


eval_block(t_block(Statement), NEnv, NewEnv)
eval_block(t_block(Statement), NEnv, NewEnv)


eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).

eval_string(t_string(S), S).
eval_integer(t_integer(I), I).
eval_float(t_float(F), F).

eval_boolean(t_boolean(true), _, _, true).
eval_boolean(t_boolean(false), _, _, false).