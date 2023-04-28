:- table condition/3.

ternary(t_ternary(Condition, Expression1, Expression2)) --> condition(Condition), [?], expression(Expression1), [:], expression(Expression2).

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

eval_ternary(t_ternary(Condition, Expression1, _), Env, ReturnVal) :-
    eval_condition(Condition, Env, true),
    eval_expr(Expression1, Env, ReturnVal).

eval_ternary(t_ternary(Condition, _, Expression2), Env, ReturnVal) :-
    eval_condition(Condition, Env, false),
    eval_expr(Expression2, Env, ReturnVal).

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


% Ternary Test cases
% ?- ternary(T, [1, <, 5, ?, 10, :, 50], []).
% T = t_ternary(t_cond_expr(t_integer(1), <, t_integer(5)), t_integer(10), t_integer(50)).

% ?- ternary(T, [1, <, 5, ?, 10, :, 50], []), eval_ternary(T, [], Val).
% T = t_ternary(t_cond_expr(t_integer(1), <, t_integer(5)), t_integer(10), t_integer(50)),
% Val = 10 ;
% false.

% ?- ternary(T, [10, <, 5, ?, 10, :, 50], []), eval_ternary(T, [], Val).
% T = t_ternary(t_cond_expr(t_integer(10), <, t_integer(5)), t_integer(10), t_integer(50)),
% Val = 50.


% ?- ternary(T, [10, <, 5, '||', 10, <=, 20,  ?, 10, :, 50], []), eval_ternary(T, [], Val).
% T = t_ternary(t_cond_cond(t_cond_expr(t_integer(10), <, t_integer(5)), '||', t_cond_expr(t_integer(10), <=, t_integer(20))), t_integer(10), t_integer(50)),
% Val = 10 ;
% false.

% ?- ternary(T, [10, <, 5, &&, 10, <=, 20,  ?, 10, :, 50], []), eval_ternary(T, [], Val).
% T = t_ternary(t_cond_cond(t_cond_expr(t_integer(10), <, t_integer(5)), &&, t_cond_expr(t_integer(10), <=, t_integer(20))), t_integer(10), t_integer(50)),
% Val = 50 .