:- table block/3.

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

% Declaration Tree
declaration(t_declare_var(Type, Variable)) --> type(Type), variable(Variable).

% Type tree
type(int) --> [int].
type(string) --> [string].
type(bool)--> [bool].

% Variable Tree
variable(t_var_id(Identifier)) --> identifier(Identifier).

% Identifier Tree
identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.

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

% Identifier
get_identifier(t_identifier(I), I).