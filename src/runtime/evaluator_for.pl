statement(t_statement(ForLoop)) --> for_loop(ForLoop).
for_loop(t_for_loop(Identifier, ForInteger, Condition, Assignment, Block)) --> [for], ['('], identifier(Identifier), [=], for_integer(ForInteger), [;], condition(Condition), [;], assignment(Assignment), [')'], ['{'], block(Block), ['}'].
for_integer(t_for_integer(Integer)) --> integer(Integer).
for_integer(t_for_integer(Identifier)) --> identifier(Identifier).

eval_statement(t_statement(ForLoop)) :- eval_for_loop(ForLoop, Env, NEnv).
eval_for_loop(t_for_loop(Identifier, ForInteger, Condition, Assignment, Block), Env, NEnv) :-
    
