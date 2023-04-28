convert(File) :-
    open(File, read, Stream),
    read_lines(Stream, TokenList),
    write(TokenList),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes),
    read_lines(Stream, Lines).
