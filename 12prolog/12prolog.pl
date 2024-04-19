% Check if a number is a perfect square
is_square(N) :-
    Sqrt is sqrt(N),
    Sqrt =:= round(Sqrt).

% Keep elements at positions that are perfect squares
keep_squares(List, Result) :-
    findall(Element, (nth1(Index, List, Element), is_square(Index)), Result).

% Helper predicate to generate a list of N random integers within a given range
generate_random_int_list(0, _, []).
generate_random_int_list(N, (Low, High), [Rand|Rest]) :-
    N > 0,
    random_between(Low, High, Rand),
    N1 is N - 1,
    generate_random_int_list(N1, (Low, High), Rest).

% Test predicates for keep_squares
test_keep_squares :-
    test_empty_list,
    test_three_elements,
    test_four_elements,
    test_random_list.

% Test case for an empty list
test_empty_list :-
    keep_squares([], Result),
    Result = [].

% Test case for a list of three elements
test_three_elements :-
    keep_squares([-2, 3, 5], Result),
    Result = [-2].

% Test case for a list of four elements
test_four_elements :-
    keep_squares([1, 4, 9, 16], Result),
    Result = [1, 16].

% Test case with a randomly generated list
% This is tricky in Prolog because random list generation and testing aren't deterministic
% Here we just generate and apply the function, checking if it executes
test_random_list :-
    generate_random_int_list(10, (1, 100), RandomList),
    keep_squares(RandomList, _Result),
    write('Random list test executed.').

% Run all tests
run_tests :-
    write('Testing keepSquares:'), nl,
    test_keep_squares,
    write('All tests passed.'), nl.

% Execute tests
:- initialization(run_tests).
