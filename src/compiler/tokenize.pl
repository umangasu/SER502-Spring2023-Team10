consult_file(File) :-
    consult(File).

convert(File) :-
    open(File, read, Stream),
    read_lines(Stream, TokenList),
    close(Stream),
    consult('../runtime/compiler.pl'),
    program(Tree, TokenList, []),
    write_tree_to_file(Tree, 'tree.txt'),
    eval_program(Tree, [],NEnv).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Result|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes),
    convert_to_integer(Line,Result),
    read_lines(Stream, Lines).

convert_to_integer(String, Result) :-
    (   atom_number(String, Number),
        integer(Number)
    ->  Result = Number
    ;   Result = String
    ).
 
write_tree_to_file(Tree, FileName) :-
    open(FileName, write, Stream, []),
    write(Stream, Tree),
    close(Stream).
