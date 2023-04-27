:- use_rendering(svgtree).

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

declaration(t_single_decl(Type, Variable)) --> type(Type), variable(Variable).

type(t_type(int)) --> [int].
type(t_type(string)) --> [string].

variable(t_variable(Identifier)) --> identifier(Identifier).

identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.


eval_decl(t_single_decl(t_type(X), Variable), Env, NewEnv) :- 
          eval_var(Variable, I), 
          update(X, I, 0, Env, NewEnv).


eval_var(t_variable(Identifier), I) :- get_identifier(Identifier, I).

get_identifier(t_identifier(I), I).






