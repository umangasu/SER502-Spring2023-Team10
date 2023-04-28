:- table block/3.
:- table expression/3.
:- table term/3.
:- table factor/3.

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

%%%%%%%%%%%%%%%%%%%%%%%%%

% Program Tree
program(t_program(program, '{', '}')) --> [program], ['{'], ['}'].
program(t_program(program, '{', Block, '}')) --> [program], ['{'], block(Block), ['}'].

% Block Tree
block(t_block_single(Statement)) --> statement(Statement).
block(t_block(Block, Statement)) --> block(Block), statement(Statement).

% Statement Tree
statement(t_statement_declaration(Declaration)) --> declaration(Declaration), [;].
statement(t_statement_assignment(Assignment)) --> assignment(Assignment), [;].

% Declaration Tree
declaration(t_declare_var(Type, Variable)) --> type(Type), variable(Variable).

% Type tree
type(int) --> [int].
type(string) --> [string].
type(bool)--> [bool].

% Variable Tree
variable(t_var_id(Identifier)) --> identifier(Identifier).

% Assignment Tree
assignment(t_assign_expr(Identifier, Expression)) --> identifier(Identifier), [=], expression(Expression).
% assignment(t_assign_ternary(Identifier, Ternary)) --> identifier(Identifier), [=], ternary(Ternary).
assignment(t_assign_plus(Identifier)) --> identifier(Identifier), [++].
assignment(t_assign_minus(Identifier)) --> identifier(Identifier), [--].

% Identifier Tree
identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.

%
integer(t_integer(N)) --> [N], {integer(N)}.

% Expression Tree
expression(t_add(Expression, Term)) --> expression(Expression), [+], term(Term).
expression(t_sub(Expression, Term)) --> expression(Expression), [-], term(Term).
expression(Term) --> term(Term).

term(t_mul(Term, Factor)) --> term(Term), [*], factor(Factor).
term(t_div(Term, Factor)) --> term(Term), [/], factor(Factor).
term(Factor) --> factor(Factor).

factor(Integer) --> integer(Integer).
factor(Identifier) --> identifier(Identifier).
factor(t_par('(', Expression, ')')) --> ['('], expression(Expression), [')'].

% Program Evaluator
eval_program(t_program(program, '{', '}'), _, _).
eval_program(t_program(program, '{', Block, '}'), Env, NEnv) :-
    eval_block(Block, Env, NEnv).

% Block Evaluator
eval_block(t_block_single(Statement), Env, NEnv) :- eval_statement(Statement, Env, NEnv).
eval_block(t_block(Block, Statement), Env, NEnv) :- 
    eval_block(Block, Env, Env1), 
    eval_statement(Statement, Env1, NEnv).


% Statement Evaluator
eval_statement(t_statement_declaration(Declaration), Env, NEnv) :- eval_declaration(Declaration, Env, NEnv).
eval_statement(t_statement_assignment(Assignment), Env, NEnv) :- eval_assign(Assignment, Env, NEnv).

% Declaration Evaluator
eval_declaration(t_declare_var(Type, Variable), Env, NEnv) :-
    eval_variable(Variable, Type, Env, NEnv).

% Variable Evaluator
eval_variable(t_var_id(Identifier), int, Env, NEnv) :- 
    get_identifier( Identifier, I),
    update(int, I, 0, Env, NEnv).

eval_variable(t_var_id(Identifier), string, Env, NEnv) :- 
    get_identifier( Identifier, I),
    update(string, I, '', Env, NEnv).

eval_variable(t_var_id(Identifier), bool, Env, NEnv) :- 
    get_identifier( Identifier, I),
    update(bool, I, false, Env, NEnv).


% Assignment Evaluator
eval_assign(t_assign_expr(Identifier, Expression), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_expr(Expression, Env, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

% eval_assign(t_assign_ternary(Identifier, Ternary), Env, NewEnv) :-
%     get_identifier(Identifier, I),
%     eval_ternary(Ternary, Env, Val),
%     ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
%     (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).
                           
eval_assign(t_assign_plus(Identifier), Env, NewEnv) :-
    get_identifier(Identifier, I),
    ((check_in_env(I, Env), eval_identifier(Identifier, Env, Val), NewVal is Val + 1, update(_, I, NewVal, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_minus(Identifier), Env, NewEnv) :-
    get_identifier(Identifier, I),
    ((check_in_env(I, Env), eval_identifier(Identifier, Env, Val), NewVal is Val - 1, update(_, I, NewVal, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).


% Identifier
eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).
get_identifier(t_identifier(I), I).

%
eval_integer(t_integer(N), N).


% Expression Evaluator
eval_expr(t_add(Expression, Term), Env, Val) :-
    eval_expr(Expression, Env, Val_expr),
    eval_term(Term, Env, Val_term),
    Val is Val_expr + Val_term.

eval_expr(t_sub(Expression, Term), Env, Val) :-
    eval_expr(Expression, Env, Val_expr),
    eval_term(Term, Env, Val_term),
    Val is Val_expr - Val_term.

eval_expr(Term, Env, Val) :- eval_term(Term, Env, Val).

eval_term(t_mul(Term, Factor), Env, Val) :-
    eval_term(Term, Env, Val_term),
    eval_factor(Factor, Env, Val_fact),
    Val is Val_term * Val_fact.

eval_term(t_div(Term, Factor), Env, _) :-
    eval_term(Term, Env, _),
    eval_factor(Factor, Env, Val_fact),
    Val_fact = 0,
    write("Can not divide by 0"),
    fail.

eval_term(t_div(Term, Factor), Env, Val) :-
    eval_term(Term, Env, Val_term),
    eval_factor(Factor, Env, Val_fact),
    Val_fact \= 0,
    Val is Val_term / Val_fact.

eval_term(Factor, Env, Val) :- eval_factor(Factor, Env, Val).

eval_factor(t_integer(N), _, N) :- eval_integer(t_integer(N), N).
eval_factor(Identifier, Env, Val) :- eval_identifier(Identifier, Env, Val).
eval_factor(t_par('(', Expression, ')'), Env, Val) :- eval_expr(Expression, Env, Val).


