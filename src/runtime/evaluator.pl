:- use_rendering(svgtree).
:- table block/3.

program(t_program(program, '{', '}')) --> [program], ['{'], ['}'].
program(t_program(program, '{', Block, '}')) --> [program], ['{'], block(Block), ['}'].
block(t_block_single(Statement)) --> statement(Statement).
block(t_block(Block, Statement)) --> block(Block), statement(Statement).

statement(t_statement_declaration(Declaration, ;)) --> declaration(Declaration), [;].
statement(t_statement_assignment(Assignment, ;)) --> assignment(Assignment), [;].

declaration(t_declaration(Type, Variable)) --> type(Type), variable(Variable).
assignment(t_assign_expr(Identifier, Expression)) --> identifier(Identifier), [=], expression(Expression).
% assignment(t_assign_ternary(Identifier, Ternary)) --> identifier(Identifier), [=], ternary(Ternary).
assignment(t_assign_plus(Identifier)) --> identifier(Identifier), [++].
assignment(t_assign_minus(Identifier)) --> identifier(Identifier), [--].

type(t_type_int(int)) --> [int].
type(t_type_float(float)) --> [float].
type(t_type_char(char)) --> [char].
type(t_type_string(string)) --> [string].
type(t_type_bool(bool)) --> [bool].

variable(t_variable(Identifier)) --> identifier(Identifier).
expression(Term) --> term(Term).
term(Factor) --> factor(Factor).
factor(Integer) --> integer(Integer).
integer(t_integer(N)) --> [N], {integer(N)}.
identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.

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

check_in_env(V, [(_, V, _) | _]). 
check_in_env(V, [(_, V1, _) | T]) :-
    V1 \= V,
    check_in_env(V, T).


eval_program(t_program(program, '{', '}'), _, _).
eval_program(t_program(program, '{', Block, '}'), Env, NEnv) :-
    eval_block(Block, Env, NEnv).

eval_block(t_block_single(Statement), Env, NEnv) :- eval_statement(Statement, Env, NEnv).
eval_block(t_block(Block, Statement), Env, NEnv) :- eval_block(Block, Env, Env1), eval_statement(Statement, Env1, NEnv).

eval_statement(t_statement_declaration(Declaration, ;), Env, NEnv) :- eval_declaration(Declaration, Env, NEnv).
eval_statement(t_statement_assignment(Assignment, ;), Env, NEnv) :- eval_assign(Assignment, Env, NEnv).
eval_declaration(t_declaration(Type, Variable), Env, NEnv) :-
    eval_type(Type, DataType),
    eval_variable(Variable, Identifier, Env, Env1),
    update(DataType, Identifier, 0, Env1, NEnv).

eval_assign(t_assign_expr(Identifier, Expression), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_expr(Expression, Env, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_plus(Identifier), Env, NewEnv) :-
    get_identifier(Identifier, I),
    ((check_in_env(I, Env), eval_identifier(Identifier, Env, Val), NewVal is Val + 1, update(_, I, NewVal, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_minus(Identifier), Env, NewEnv) :-
    get_identifier(Identifier, I),
    ((check_in_env(I, Env), eval_identifier(Identifier, Env, Val), NewVal is Val - 1, update(_, I, NewVal, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_variable(t_variable(Variable), V, Env, Env) :- eval_identifier(Variable, V, Env).
eval_expr(Term, Env, Val) :- eval_term(Term, Env, Val).
eval_term(Factor, Env, Val) :- eval_factor(Factor, Env, Val).
eval_factor(t_integer(N), _, N) :- eval_integer(t_integer(N), N).
eval_integer(t_integer(N), N).

eval_type(t_type_int(X), X).
eval_type(t_type_float(X), X).
eval_type(t_type_char(X), X).
eval_type(t_type_string(X), X).
eval_type(t_type_bool(X), X).

eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).
get_identifier(t_identifier(I), I).

