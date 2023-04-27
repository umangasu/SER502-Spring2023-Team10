:- use_rendering(svgtree).

:- table expression/3.
:- table term/3.
:- table factor/3.

%%%%%%%%%%

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



%%%%%%%%%%

expression(t_add(Expression, Term)) --> expression(Expression), [+], term(Term).
expression(t_sub(Expression, Term)) --> expression(Expression), [-], term(Term).
expression(Term) --> term(Term).

term(t_mul(Term, Factor)) --> term(Term), [*], factor(Factor).
term(t_div(Term, Factor)) --> term(Term), [/], factor(Factor).
term(Factor) --> factor(Factor).
 
factor(Integer) --> integer(Integer).
factor(Float) --> float(Float).
factor(Identifier) --> identifier(Identifier).
factor(t_par('(', Expression, ')')) --> ['('], expression(Expression), [')'].

identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
integer(t_integer(N)) --> [N], {integer(N)}.
float(t_float(F)) --> [F], {float(F)}.

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

eval_factor(Identifier, Env, Val) :- eval_identifier(Identifier, Env, Val).
eval_factor(t_integer(N), _, N) :- eval_integer(t_integer(N), N).
eval_factor(t_float(F), _, F) :- eval_float(t_float(F), F).
eval_factor(t_par('(', Expression, ')'), Env, Val) :- eval_expr(Expression, Env, Val).

eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).
eval_integer(t_integer(N), N).
eval_float(t_float(F), F).


% Test cases
% expression(T, [x, + ,y], []) , eval_expr(T, [(a, x, 10), (a, y, 1)], Val).
% expression(T, [10, + ,5], []) , eval_expr(T, [], Val).
% expression(T, [10, - ,5], []) , eval_expr(T, [], Val).
% expression(T, [10, / ,5], []) , eval_expr(T, [], Val).
% expression(T, [10, * ,5], []) , eval_expr(T, [], Val).
% expression(T, [10, * ,5, +, 50], []) , eval_expr(T, [], Val).