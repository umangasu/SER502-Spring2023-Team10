for_loop(t_for_loop(Assignment1, Condition, Assignment2, Block)) --> [for], ['('], assignment(Assignment1), [;], condition(Condition), [;], assignment(Assignment2), [')'], ['{'], block(Block), ['}'].

eval_for_loop(t_for_loop(Assignment1, Condition, _Assignment2, _Block), Env, NEnv) :- 
    eval_assign(Assignment1, Env, NEnv),
    eval_condition(Condition, NEnv, false).

eval_for_loop(t_for_loop(Assignment1, Condition, Assignment2, Block), Env, NEnv) :- 
    eval_assign(Assignment1, Env, Env1),
    eval_condition(Condition, Env1, true),
    eval_block(Block, Env1, Env2),
    eval_loop(Condition, Assignment2, Block, Env2, NEnv).

eval_loop(Condition, Assignment, _Block, Env, NEnv) :-
    eval_assign(Assignment, Env, NEnv),
    eval_condition(Condition, NEnv, false).

eval_loop(Condition, Assignment, Block, Env, NEnv) :-
    eval_assign(Assignment, Env, Env1),
    eval_condition(Condition, Env1, true),
    eval_block(Block, Env1, Env2),
    eval_loop(Condition, Assignment, Block, Env2, NEnv).

program(Tree, [program, '{', int, i, ;, int, j, ;, j, =, 0, ;, for, '(', i, =, 0, ;, i, <, 4, ;, i, ++, ')', '{', j, ++, ;, '}', '}'], []), eval_program(Tree, [], Env).
