:- include('../../src/compiler/grammar.pl').

test_case(1, [program, '{', '}'], []).
test_case(2, [program, '{', int, x, ;,'}'], []).
test_case(3, [program, '{', int, x, =, 8, ;, int, y, ;, int, z, ;, z, =, 0, ;, '}'], []).

run(Case) :-
    test_case(Case, Tokens, Remaining),
    write('\nTest case '), write(Case), write(':\n\n'),
    (current_predicate(program/2) ->
        findall((Tree), (program(Tree, Tokens, Remaining)), Ans_Set), 
        forall(member(Ans, Ans_Set), (write(Ans), nl));
        write('\n***** Missing program/2 *****\n\n')).

run :- findall((Case), run(Case), _), halt.
