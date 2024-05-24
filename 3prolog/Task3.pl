% Визначення скінченого автомата
/*states([0, 1, 2, 3]).
symbols([b, a]).
transition(0, b, 1).
transition(0, a, 2).
transition(1, b, 3).
transition(1, a, 1).
transition(2, b, 2).
transition(2, a, 0).
transition(3, b, 1).
transition(3, a, 3).
startState(0).
finalStates([3]).*/

states([0, 1, 2, 3]).
symbols([b, a, c]).
transition(0, b, 1).
transition(0, a, 2).
transition(1, b, 3).
transition(1, a, 1).
transition(2, b, 2).
transition(2, c, 2).
transition(2, a, 3).
transition(2, a, 0).
transition(3, b, 1).
transition(3, c, 2).
transition(3, a, 3).
transition(3, b, 3).
startState(0).
finalStates([3, 2]).

% Перевірка, чи символ належить алфавіту автомата
symbol(Symbol) :-
    symbols(Symbols),
    member(Symbol, Symbols).

% Перевірка, чи приймається слово автоматом
accept_word(Word) :- 
    startState(StartState),
    accept_word_helper(StartState, Word).

% Допоміжний предикат для accept_word
accept_word_helper(State, []) :- 
    finalStates(FinalStates),
    memberchk(State, FinalStates).
accept_word_helper(State, [Symbol|Word]) :- 
    transition(State, Symbol, NextState),
    accept_word_helper(NextState, Word).

findXXX(X, XXX) :-
    append(X, X, XX),
    append(XX, X, XXX),
    (accept_word(XXX) ->
        true
    ;
        fail
    ).

main :-
    writeln('Введіть слово X:'),
    read(XAtom),  % Користувач вводить слово X як атом
    atom_chars(XAtom, XList),  % Конвертує атом у список символів
    (findXXX(XList, XXX) ->
        format('Знайдене слово XXX: ~s\n', [XXX])
    ;
        writeln('Слово XXX не знайдено.')
    ).
