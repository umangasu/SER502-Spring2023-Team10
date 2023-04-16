:- include('../../src/compiler/grammar.pl').

test_case(1, [program, '{', '}'], []).

run(Case) :-
    test_case(Case, Tokens, Expected),
    write('\nTest case '), write(Case), write(':\n\n'),
    (current_predicate(program/2) ->
        findall((R), (program(Tokens, R)), Ans_Set), 
        forall(member(Ans, Ans_Set), (Ans = Expected -> write("pass"),nl ; write("fail. Expected: "), write(Expected), nl));
        write('\n***** Missing program/2 *****\n\n')).

run :- findall((Case), run(Case), _), halt.
