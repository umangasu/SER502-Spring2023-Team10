statement(t_statement_while(WhileLoop)) --> while_loop(WhileLoop).
while_loop(t_while(Condition, Block)) --> [while], ['('], condition(Condition), [')'], ['{'], block(Block), ['}'].

eval_statement(t_statement_while(WhileLoop), Env, NEnv) :- eval_while(WhileLoop, Env, NEnv).

eval_while(t_while(Condition, _), Env, Env) :-
    eval_condition(Condition, Env, false).

eval_while(t_while(Condition, Block), Env, NEnv) :- 
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, Env1),
    eval_while(t_while(Condition, Block), Env1, NEnv).