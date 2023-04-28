
:- table block/3.

:- table expression/3.
:- table term/3.
:- table factor/3.

:- table condition/3.

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

check_in_env(V, [(_, V, _) | _]).
check_in_env(V, [(_, V1, _) | T]) :-
    V1 \= V,
    check_in_env(V, T).

%%%%%%%%%%%%%%%%%%%%%%%%%

% Program Tree
program(t_program(program, '{', '}')) --> [program], ['{'], ['}'].
program(t_program(program, '{', Block, '}')) --> [program], ['{'], block(Block), ['}'].

% Block Tree
block(t_block_single(Statement)) --> statement(Statement).
block(t_block(Block, Statement)) --> block(Block), statement(Statement).

% Statement Tree
statement(t_statement_declaration(Declaration)) --> declaration(Declaration), [;].
statement(t_statement_assignment(Assignment)) --> assignment(Assignment), [;].
statement(t_statement_print(PrintStatement)) --> print_statement(PrintStatement), [;].
statement(t_statement_if(IfStatement)) --> if_statement(IfStatement).
statement(t_statement_while(WhileLoop)) --> while_loop(WhileLoop).
statement(t_statement_for(ForLoop)) --> for_loop(ForLoop).
statement(t_statement_range(ForRange)) --> for_range(ForRange).

% Declaration Tree
declaration(t_declare_var(Type, Variable)) --> type(Type), variable(Variable).

% Type tree
type(int) --> [int].
type(string) --> [string].
type(bool)--> [bool].

% Variable Tree
variable(t_var_id(Identifier)) --> identifier(Identifier).
% variable(t_variable(Assignment)) --> assignment(Assignment).

% Assignment Tree
assignment(t_assign_expr(Identifier, Expression)) --> identifier(Identifier), [=], expression(Expression).
assignment(t_assign_str(Identifier, String)) --> identifier(Identifier), [=], string(String).
assignment(t_assign_bool(Identifier, Boolean)) --> identifier(Identifier), [=], boolean(Boolean).
assignment(t_assign_ternary(Identifier, Ternary)) --> identifier(Identifier), [=], ternary(Ternary).
assignment(t_assign_plus(Identifier)) --> identifier(Identifier), [++].
assignment(t_assign_minus(Identifier)) --> identifier(Identifier), [--].

% Identifier Tree
identifier(t_identifier(I)) --> [I], {atom(I), \+ member(I, [program, for, if, else, while, range, print, int, float, char, string, bool, in, true, false])}.

%
integer(t_integer(N)) --> [N], {integer(N)}.
string(t_string(S)) --> ['"'], [S], ['"'], {atom(S)}.
boolean(t_boolean(true)) --> [true].
boolean(t_boolean(false)) --> [false].

% Ternary Tree
ternary(t_ternary(Condition, Expression1, Expression2)) --> condition(Condition), [?], expression(Expression1), [:], expression(Expression2).

% Expression Tree
expression(t_add(Expression, Term)) --> expression(Expression), [+], term(Term).
expression(t_sub(Expression, Term)) --> expression(Expression), [-], term(Term).
expression(Term) --> term(Term).

term(t_mul(Term, Factor)) --> term(Term), [*], factor(Factor).
term(t_div(Term, Factor)) --> term(Term), [/], factor(Factor).
term(Factor) --> factor(Factor).

factor(Integer) --> integer(Integer).
factor(Boolean) --> boolean(Boolean).
factor(Identifier) --> identifier(Identifier).
factor(t_par('(', Expression, ')')) --> ['('], expression(Expression), [')'].

% If Tree
if_statement(t_if_parent(Condition, Block, IfStatement1)) --> [if], ['('], condition(Condition), [')'], ['{'], block(Block), ['}'], if_statement1(IfStatement1).
if_statement1(t_if()) --> [].
if_statement1(t_else(Block)) --> [else], ['{'], block(Block), ['}'].
if_statement1(t_else_if(IfStatement)) --> [else], if_statement(IfStatement).

% Condition Tree
condition(t_cond_expr(Expression1, RelationOp, Expression2)) --> expression(Expression1), relation_op(RelationOp), expression(Expression2).
condition(t_cond_cond(Condition1, LogicalOp, Condition2)) --> condition(Condition1), logical_op(LogicalOp), condition(Condition2).
condition(t_cond_bool(Boolean)) --> boolean(Boolean).
condition(t_cond_negate(Condition)) --> [!], condition(Condition).

% Relation Tree
relation_op(<) --> [<].
relation_op(<=) --> [<=].
relation_op(>) --> [>].
relation_op(>=) --> [>=].
relation_op(==) --> [==].
relation_op('!') --> ['!='].

% Logical Tree
logical_op('&&') --> ['&&'].
logical_op('||') --> ['||'].

% While Tree
while_loop(t_while(Condition, Block)) --> [while], ['('], condition(Condition), [')'], ['{'], block(Block), ['}'].


% For Tree
for_loop(t_for_loop(Assignment1, Condition, Assignment2, Block)) --> [for], ['('], assignment(Assignment1), [;], condition(Condition), [;], assignment(Assignment2), [')'], ['{'], block(Block), ['}'].

% For Range Tree
for_range(t_for_range(Identifier, ForInteger1, ForInteger2, Block)) --> [for], identifier(Identifier), [in], [range], ['('], for_integer(ForInteger1), [';'], for_integer(ForInteger2), [')'], ['{'], block(Block), ['}'].

for_integer(t_for_int(Integer)) --> integer(Integer).
for_integer(t_for_id(Identifier)) --> identifier(Identifier).

% Print Tree

print_statement(t_print_statement(PrintValues)) --> [print], ['('], print_values(PrintValues), [')'].
print_values(t_print_values(t_print_identifier(Identifier), PrintValues)) --> identifier(Identifier), [','], print_values(PrintValues).
print_values(t_print_values(t_print_string(String), PrintValues)) --> string(String), [','], print_values(PrintValues).
print_values(t_print_values(t_print_int(Integer), PrintValues)) --> integer(Integer), [','], print_values(PrintValues).
print_values(t_print_int(Integer)) --> integer(Integer).
print_values(t_print_string(String)) --> string(String).
print_values(t_print_identifier(Identifier)) --> identifier(Identifier).


% Program Evaluator
eval_program(t_program(program, '{', '}'), _, _).
eval_program(t_program(program, '{', Block, '}'), Env, NEnv) :-
    eval_block(Block, Env, NEnv).

% Block Evaluator
eval_block(t_block_single(Statement), Env, NEnv) :- eval_statement(Statement, Env, NEnv).
eval_block(t_block(Block, Statement), Env, NEnv) :-
    eval_block(Block, Env, Env1),
    eval_statement(Statement, Env1, NEnv).


% Statement Evaluator
eval_statement(t_statement_declaration(Declaration), Env, NEnv) :- eval_declaration(Declaration, Env, NEnv).
eval_statement(t_statement_assignment(Assignment), Env, NEnv) :- eval_assign(Assignment, Env, NEnv).
eval_statement(t_statement_print(PrintStatement), Env, Env) :- eval_print_statement(PrintStatement, Env).
eval_statement(t_statement_if(IfStatement), Env, NEnv) :- eval_if_statement(IfStatement, Env, NEnv).
eval_statement(t_statement_while(WhileLoop), Env, NEnv) :- eval_while(WhileLoop, Env, NEnv).
eval_statement(t_statement_for(ForLoop), Env, NEnv) :- eval_for_loop(ForLoop, Env, NEnv).
eval_statement(t_statement_range(ForRange), Env, NEnv) :- eval_for_range(ForRange, Env, NEnv).

% Declaration Evaluator
eval_declaration(t_declare_var(Type, Variable), Env, NEnv) :-
    eval_variable(Variable, Type, Env, NEnv).

% Variable Evaluator
eval_variable(t_var_id(Identifier), int, Env, NEnv) :-
    get_identifier( Identifier, I),
    update(int, I, 0, Env, NEnv).

eval_variable(t_var_id(Identifier), string, Env, NEnv) :-
    get_identifier( Identifier, I),
    update(string, I, '', Env, NEnv).

eval_variable(t_var_id(Identifier), bool, Env, NEnv) :-
    get_identifier( Identifier, I),
    update(bool, I, false, Env, NEnv).


% Assignment Evaluator
eval_assign(t_assign_expr(Identifier, Expression), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_expr(Expression, Env, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_str(Identifier, String), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_string(String, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_bool(Identifier, Boolean), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_bool(Boolean, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_ternary(Identifier, Ternary), Env, NewEnv) :-
    get_identifier(Identifier, I),
    eval_ternary(Ternary, Env, Val),
    ((check_in_env(I, Env), update(_, I, Val, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_plus(Identifier), Env, NewEnv) :-
    get_identifier(Identifier, I),
    ((check_in_env(I, Env), eval_identifier(Identifier, Env, Val), NewVal is Val + 1, update(_, I, NewVal, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).

eval_assign(t_assign_minus(Identifier), Env, NewEnv) :-
    get_identifier(Identifier, I),
    ((check_in_env(I, Env), eval_identifier(Identifier, Env, Val), NewVal is Val - 1, update(_, I, NewVal, Env, NewEnv));
    (\+ check_in_env(I, Env) , write("Variable do not exist"), fail)).


% Identifier
eval_identifier(t_identifier(I), Env, Val) :- lookup(I, Env, Val).
get_identifier(t_identifier(I), I).

%
eval_integer(t_integer(N), N).
eval_string(t_string(S), S).
eval_bool(t_boolean(B), B).

% Ternary Evaluator

eval_ternary(t_ternary(Condition, Expression1, _), Env, ReturnVal) :-
    eval_condition(Condition, Env, true),
    eval_expr(Expression1, Env, ReturnVal).

eval_ternary(t_ternary(Condition, _, Expression2), Env, ReturnVal) :-
    eval_condition(Condition, Env, false),
    eval_expr(Expression2, Env, ReturnVal).

% Expression Evaluator
eval_expr(t_add(Expression, Term), Env, Val) :-
    eval_expr(Expression, Env, Val_expr),
    eval_term(Term, Env, Val_term),
    Val is Val_expr + Val_term.

eval_expr(t_sub(Expression, Term), Env, Val) :-
    eval_expr(Expression, Env, Val_expr),
    eval_term(Term, Env, Val_term),
    Val is Val_expr - Val_term.

eval_expr(Term, Env, Val) :- eval_term(Term, Env, Val).

eval_term(t_mul(Term, Factor), Env, Val) :-
    eval_term(Term, Env, Val_term),
    eval_factor(Factor, Env, Val_fact),
    Val is Val_term * Val_fact.

eval_term(t_div(Term, Factor), Env, _) :-
    eval_term(Term, Env, _),
    eval_factor(Factor, Env, Val_fact),
    Val_fact = 0,
    write("Can not divide by 0"),
    fail.

eval_term(t_div(Term, Factor), Env, Val) :-
    eval_term(Term, Env, Val_term),
    eval_factor(Factor, Env, Val_fact),
    Val_fact \= 0,
    Val is Val_term / Val_fact.

eval_term(Factor, Env, Val) :- eval_factor(Factor, Env, Val).

eval_factor(t_integer(N), _, N) :- eval_integer(t_integer(N), N).
eval_factor(t_boolean(N), _, N) :- eval_bool(t_boolean(N), N).
eval_factor(Identifier, Env, Val) :- eval_identifier(Identifier, Env, Val).
eval_factor(t_par('(', Expression, ')'), Env, Val) :- eval_expr(Expression, Env, Val).

% If Evaluator

eval_if_statement(t_if_parent(Condition, Block, _), Env, NEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, NEnv).

eval_if_statement(t_if_parent(Condition, _, IfStatement1), Env, NEnv) :-
    eval_condition(Condition, Env, false),
    eval_if_statement1(IfStatement1, Env, NEnv).

eval_if_statement1(t_if(), Env, Env).

eval_if_statement1(t_else(Block), Env, NEnv) :-
    eval_block(Block, Env, NEnv).

eval_if_statement1(t_else_if(IfStatement), Env, NEnv) :-
    eval_if_statement(IfStatement, Env, NEnv).

eval_condition(t_cond_negate(Condition), Env, true) :-
    eval_condition(Condition, Env, false).

eval_condition(t_cond_negate(Condition), Env, false) :-
    eval_condition(Condition, Env, true).

% Condition Evaluator

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

eval_condition(t_cond_bool(Boolean), _, Value) :-
    eval_bool(Boolean, Value).

% While Evaluator
eval_while(t_while(Condition, _), Env, Env) :-
    eval_condition(Condition, Env, false).

eval_while(t_while(Condition, Block), Env, NEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, Env1),
    eval_while(t_while(Condition, Block), Env1, NEnv).

% For loop Evaluator

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


% For Range Evaluator

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

% Print Evaluator
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
eval_print_values(t_print_values(t_print_bool(t_boolean(B)), PrintValues), Env) :-
    write(B),
    write(" "),
    eval_print_values(PrintValues , Env).

eval_print_values(t_print_int(t_integer(N)), _) :- write(N).
eval_print_values(t_print_string(t_string(S)), _) :- write(S).
eval_print_values(t_print_bool(t_boolean(B)), _) :- write(B).
eval_print_values(t_print_identifier(I), Env) :- eval_identifier(I, Env, IVal), write(IVal).

