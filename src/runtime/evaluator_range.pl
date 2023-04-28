for_range(t_for_range(Identifier, ForInteger1, ForInteger2, Block)) --> [for], identifier(Identifier), [in], [range], ['('], for_integer(ForInteger1), [';'], for_integer(ForInteger2), [')'], ['{'], block(Block), ['}'].
for_integer(t_for_int(Integer)) --> integer(Integer).
for_integer(t_for_id(Identifier)) --> identifier(Identifier).

eval_for_range(t_for_range(Identifier, ForInteger1, ForInteger2, _Block), Env, NEnv) :-
    get_identifier(Identifier, I),
    eval_for_integer(ForInteger1, Env, Start),
    update(int, I, Start, Env, NEnv),
    eval_for_integer(ForInteger2, NEnv, End),
    Start >= End.

eval_for_range(t_for_range(Identifier, ForInteger1, ForInteger2, Block), Env, NEnv) :-
    get_identifier(Identifier, I),
    eval_for_integer(ForInteger1, Env, Start),
    update(int, I, Start, Env, Env1),
    eval_for_integer(ForInteger2, Env1, End),
    Start < End,
    eval_block(Block, Env1, Env2),
    eval_range_loop(I, Start, End, Block, Env2, NEnv).

eval_range_loop(I, Start, End, _Block, Env, NEnv) :-
    Start1 is Start + 1,
    update(int, I, Start1, Env, NEnv),
    Start1 >= End.

eval_range_loop(I, Start, End, Block, Env, NEnv) :-
    Start1 is Start + 1,
    update(int, I, Start1, Env, Env1),
    Start1 < End,
    eval_block(Block, Env1, Env2),
    eval_range_loop(I, Start1, End, Block, Env2, NEnv).


eval_for_integer(t_for_int(Integer), _, N) :- eval_integer(Integer, N).
eval_for_integer(t_for_id(Identifier), Env, N) :- eval_identifier(Identifier, Env, N).
