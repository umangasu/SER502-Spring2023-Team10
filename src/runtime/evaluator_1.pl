:- use_rendering(svgtree).

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


identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, for, while, range, print, int, float, char, string, bool, in])}.
string(t_string(S)) --> ['"'], [S], ['"'], {atom(S)}.
integer(t_integer(N)) --> [N], {integer(N)}.

eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).

print_statement(t_print_statement(PrintValues)) --> [print], ['('], print_values(PrintValues), [')'].
print_values(t_print_values(t_print_identifier(Identifier), PrintValues)) --> identifier(Identifier), [','], print_values(PrintValues).
print_values(t_print_values(t_print_string(String), PrintValues)) --> string(String), [','], print_values(PrintValues).
print_values(t_print_values(t_print_int(Integer), PrintValues)) --> integer(Integer), [','], print_values(PrintValues).
print_values(t_print_int(Integer)) --> integer(Integer).
print_values(t_print_string(String)) --> string(String).
print_values(t_print_identifier(Identifier)) --> identifier(Identifier).


eval_print_statement(t_print_statement(PrintValues), Env) :- eval_print_values(PrintValues, Env), nl.
eval_print_values(t_print_values(t_print_identifier(I), PrintValues), Env) :-
    % eval_identifier(I, Env, IVal), write(IVal), 
    eval_print_values(t_print_identifier(I), Env),
    write(" "), 
    eval_print_values(PrintValues , Env).
eval_print_values(t_print_values(t_print_int(t_integer(N)), PrintValues), Env) :-
    write(N), 
    write(" "), 
    eval_print_values(PrintValues , Env).
eval_print_values(t_print_values(t_print_string(t_string(S)), PrintValues), Env) :- 
    write(S), 
    write(" "),
    eval_print_values(PrintValues , Env).
eval_print_values(t_print_int(t_integer(N)), _) :- write(N).
eval_print_values(t_print_string(t_string(S)), _) :- write(S).
eval_print_values(t_print_identifier(I), Env) :- eval_identifier(I, Env, IVal), write(IVal).
