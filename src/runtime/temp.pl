% Program Tree
program(t_program(program, '{', '}')) --> [program], ['{'], ['}'].
program(t_program(program, '{', Block, '}')) --> [program], ['{'], block(Block), ['}'].

block(_, _, _).

% block(t_block_single(Statement)) --> statement(Statement).
% block(t_block(Block, Statement)) --> block(Block), statement(Statement).


% Program Evaluator
eval_program(t_program(program, '{', '}'), _, _).
eval_program(t_program(program, '{', Block, '}'), Env, NEnv) :-
    eval_block(Block, Env, NEnv).

% Bloack Evaluator
eval_block(_, _, _).