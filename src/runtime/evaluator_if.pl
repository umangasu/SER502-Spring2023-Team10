:- table condition/3.

if_statement(t_if_parent(Condition, Block, IfStatement1)) --> [if], ['('], condition(Condition), [')'], ['{'], block(Block), ['}'], if_statement1(IfStatement1).
if_statement1(t_if()) --> [].
if_statement1(t_else(Block)) --> [else], ['{'], block(Block), ['}'].
if_statement1(t_else_if(IfStatement)) --> [else], if_statement(IfStatement).

condition(t_cond_expr(Expression1, RelationOp, Expression2)) --> expression(Expression1), relation_op(RelationOp), expression(Expression2).
condition(t_cond_cond(Condition1, LogicalOp, Condition2)) --> condition(Condition1), logical_op(LogicalOp), condition(Condition2).

relation_op(<) --> [<].
relation_op(<=) --> [<=].
relation_op(>) --> [>].
relation_op(>=) --> [>=].
relation_op(==) --> [==].
relation_op('!') --> ['!='].

logical_op('&&') --> ['&&'].
logical_op('||') --> ['||'].

expression(Term) --> term(Term).
term(Factor) --> factor(Factor).
factor(Integer) --> integer(Integer).
integer(t_integer(N)) --> [N], {integer(N)}.

eval_if_statement(t_if_parent(Condition, Block, IfStatement1), Env, NEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, Env1),
    eval_if_statement1(IfStatement1, Env1, NEnv).

eval_if_statement(t_if_parent(Condition, _, IfStatement1), Env, NEnv) :-
    eval_condition(Condition, Env, false),
    eval_if_statement1(IfStatement1, Env, NEnv).

eval_if_statement1(t_if(), Env, Env).

eval_if_statement1(t_else(Block), Env, NEnv) :-
    eval_block(Block, Env, NEnv).

eval_if_statement1(t_else_if(IfStatement), Env, NEnv) :-
    eval_if_statement(IfStatement, Env, NEnv).

eval_condition(t_cond_cond(Condition1, '&&', Condition2), Env, false) :-
    eval_condition(Condition1, Env, false);
    eval_condition(Condition2, Env, false).

eval_condition(t_cond_cond(Condition1, '&&', Condition2), Env, true) :-
    eval_condition(Condition1, Env, true),
    eval_condition(Condition2, Env, true).

eval_condition(t_cond_cond(Condition1, '||', Condition2), Env, false) :-
    eval_condition(Condition1, Env, false),
    eval_condition(Condition2, Env, false).

eval_condition(t_cond_cond(Condition1, '||', Condition2), Env, true) :-
    eval_condition(Condition1, Env, true);
    eval_condition(Condition2, Env, true).

eval_condition(t_cond_expr(Expression1, <, Expression2), Env, Value) :-
    eval_expr(Expression1, Env, Value1),
    eval_expr(Expression2, Env, Value2),
    (( Value1 < Value2, Value = true);( \+(Value1 < Value2), Value = false)).

eval_condition(t_cond_expr(Expression1, >, Expression2), Env, Value) :-
    eval_expr(Expression1, Env, Value1),
    eval_expr(Expression2, Env, Value2),
    (( Value1 > Value2, Value = true);( \+(Value1 > Value2), Value = false)).

eval_condition(t_cond_expr(Expression1, <=, Expression2), Env, Value) :-
    eval_expr(Expression1, Env, Value1),
    eval_expr(Expression2, Env, Value2),
    (( Value1 > Value2, Value = false);( \+(Value1 > Value2), Value = true)).

eval_condition(t_cond_expr(Expression1, >=, Expression2), Env, Value) :-
    eval_expr(Expression1, Env, Value1),
    eval_expr(Expression2, Env, Value2),
    (( Value1 >= Value2, Value = true);( \+(Value1 >= Value2), Value = false)).

eval_condition(t_cond_expr(Expression1, ==, Expression2), Env, Value) :-
    eval_expr(Expression1, Env, Value1),
    eval_expr(Expression2, Env, Value2),
    (( Value1 = Value2, Value = true);( \+(Value1 = Value2), Value = false)).

eval_condition(t_cond_expr(Expression1, '!=', Expression2), Env, Value) :-
    eval_expr(Expression1, Env, Value1),
    eval_expr(Expression2, Env, Value2),
    (( Value1 = Value2, Value = false);( \+(Value1 = Value2), Value = true)).

eval_expr(Term, Env, Val) :- eval_term(Term, Env, Val).
eval_term(Factor, Env, Val) :- eval_factor(Factor, Env, Val).
eval_factor(t_integer(N), _, N) :- eval_integer(t_integer(N), N).
eval_integer(t_integer(N), N).


% % Evaluate if statement
% % if_statement(+ParseTree, +Env, -NewEnv)
% if_statement(t_if_statement(_, Condition, Block, _), Env, NewEnv) :-
%     % Evaluate the condition
%     condition_eval(Condition, Env),
%     % Evaluate the block
%     block_eval(Block, Env, NewEnv).
% if_statement(t_if_statement(_, Condition, _, IfStatement1), Env, NewEnv) :-
%     % Evaluate the condition
%     not(condition_eval(Condition, Env)),
%     % Evaluate the next if statement or else block
%     if_statement1_eval(IfStatement1, Env, NewEnv).

% % Evaluate if statement 1 (else or next if statement)
% % if_statement1_eval(+ParseTree, +Env, -NewEnv)
% if_statement1_eval(t_if_statement1(), Env, Env).
% if_statement1_eval(t_if_statement1(_, Block), Env, NewEnv) :-
%     % Evaluate the block
%     block_eval(Block, Env, NewEnv).
% if_statement1_eval(t_if_statement1(_, IfStatement), Env, NewEnv) :-
%     % Evaluate the next if statement or else block
%     if_statement_eval(IfStatement, Env, NewEnv).

% ​​program { int x; int y; if (4 < 5) { print(y); }}
