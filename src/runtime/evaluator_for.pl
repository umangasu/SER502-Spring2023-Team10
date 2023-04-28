statement(t_statement_for(ForLoop)) --> for_loop(ForLoop).
for_loop(t_for_loop(Identifier, ForInteger, Condition, Assignment, Block)) --> [for], ['('], identifier(Identifier), [=], for_integer(ForInteger), [;], condition(Condition), [;], assignment(Assignment), [')'], ['{'], block(Block), ['}'].
for_integer(t_for_int(Integer)) --> integer(Integer).
for_integer(t_for_id(Identifier)) --> identifier(Identifier).


eval_statement(t_statement_for(ForLoop), Env, NEnv) :- eval_for_loop(ForLoop, Env, NEnv).
eval_for_loop(t_for_loop(Identifier, ForInteger, Condition, _, _), Env, NEnv) :- 
    eval_for_integer(ForInteger, I, Env),
    eval_assign(t_assign_expr(Identifier, I), Env, NEnv),
    eval_condition(Condition, NEnv, false).

eval_for_loop(t_for_loop(Identifier, ForInteger, Condition, Assignment, Block), Env, NEnv) :- 
    eval_for_integer(ForInteger, I, Env),
    eval_assign(t_assign_expr(Identifier, I), Env, Env1),
    eval_condition(Condition, Env1, true),
    eval_statement(t_statement_assignment(Assignment), Env1, Env2),
    eval_block(Block, Env2, Env3),
    eval_for_loop(t_for_loop(Identifier, ForInteger, Condition, Assignment, Block), Env3, NEnv).

eval_for_integer(t_for_int(Integer), Integer, _).
eval_for_integer(t_for_id(Identifier, I, Env)) :-  lookup(Identifier, Env, I).

program(Tree, [program, '{', int, i, ;, int, j, ;, j, =, 0, ;, for, '(', i, =, 0, ;, i, <, 4, ;, i, ++, ')', '{', j, ++, ;, '}', '}'], []).