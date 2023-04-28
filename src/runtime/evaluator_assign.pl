% Ternary done without testing, everything else works

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



assignment(t_assign_expr(Identifier, Expression)) --> identifier(Identifier), [=], expression(Expression).
assignment(t_assign_ternary(Identifier, Ternary)) --> identifier(Identifier), [=], ternary(Ternary).
assignment(t_assign_plus(Identifier)) --> identifier(Identifier), [++].
assignment(t_assign_minus(Identifier)) --> identifier(Identifier), [--].

expression(Term) --> term(Term).
term(Factor) --> factor(Factor).
factor(Integer) --> integer(Integer).
integer(t_integer(N)) --> [N], {integer(N)}.

identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.

eval_assign(t_assign_expr(Identifier, Expression), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_expr(Expression, Env, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_ternary(Identifier, Ternary), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_ternary(Ternary, Env, Val),
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

eval_expr(Term, Env, Val) :- eval_term(Term, Env, Val).
eval_term(Factor, Env, Val) :- eval_factor(Factor, Env, Val).
eval_factor(t_integer(N), _, N) :- eval_integer(t_integer(N), N).
eval_integer(t_integer(N), N).

eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).
get_identifier(t_identifier(I), I).


% ?- assignment(T, [i, ++], []), eval_assign(T, [(int, i, 0)], Val).
% T = t_assign_plus(t_identifier(i)),
% Val = [(int, i, 1)] .

% ?- assignment(T, [i, =, 5], []), eval_assign(T, [(int, i, 0)], Val).
% T = t_assign_expr(t_identifier(i), t_integer(5)),
% Val = [(int, i, 5)] .