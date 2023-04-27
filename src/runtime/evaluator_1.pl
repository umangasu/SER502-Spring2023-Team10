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

check_in_env(V, [(V, _) | _]). 
check_in_env(V, [(V1, _) | T]) :-
    V1 \= V,
    check_in_env(V, T).


string(t_string(S)) --> ['"'], [S], ['"'], {atom(S)}.
integer(t_integer(N)) --> [N], {integer(N)}.

print_statement(t_print_statement(PrintValues)) --> [print], ['('], print_values(PrintValues), [')'].
print_values(t_print_values(t_print_string(String), PrintValues)) --> string(String), [','], print_values(PrintValues).
print_values(t_print_values(t_print_int(Integer), PrintValues)) --> integer(Integer), [','], print_values(PrintValues).
print_values(t_print_int(Integer)) --> integer(Integer).
print_values(t_print_string(String)) --> string(String).


eval_print_statement(t_print_statement(PrintValues), Env) :- eval_print_values(PrintValues, Env).
eval_print_values(t_print_values(t_print_int(t_integer(N)), PrintValues), Env) :- write(N), eval_print_values(PrintValues , Env).
eval_print_values(t_print_values(t_print_string(t_string(S)), PrintValues), Env) :- write(S), eval_print_values(PrintValues , Env).
eval_print_values(t_print_int(t_integer(N)), _) :- write(N).
eval_print_values(t_print_string(t_string(S)), _) :- write(S).