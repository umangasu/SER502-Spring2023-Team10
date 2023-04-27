convert(Input) :-
    split_string(Input, "\n","", Lines),
    write(Lines).
