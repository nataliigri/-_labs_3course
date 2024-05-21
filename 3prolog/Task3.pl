% Визначення станів, символів алфавіту та переходів
states([0, 1, 2, 3]).
symbols([a, b]).
transition(0, b, 1).
transition(0, a, 2).
transition(1, b, 3).
transition(1, a, 1).
transition(2, b, 2).
transition(2, a, 0).
transition(3, b, 1).
transition(3, a, 3).
startState(0).
finalStates([3]).

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

% Предикат для знаходження слів x та xxx, які приймаються автоматом
find_x_and_xxx(MinLen, MaxLen) :-
    MinLen =< MaxLen,                      % Перевірка, що MinLen <= MaxLen
    between(MinLen, MaxLen, K),            % Генерація довжини слова у заданому діапазоні
    K > 0,                                 % Перевірка, чи K більше 0
    automata_word_length_k(K, X),          % Знаходження слова X
    append(X, X, XX),                      % Створення подвійного слова XX
    append(XX, X, XXX),                    % Створення потрійного слова XXX
    accept_word(XXX),                      % Перевірка, чи приймається XXX
    format('Знайдене слово X = ~s\n', [X]),
    format('Знайдене слово XXX = ~s\n', [XXX]),
    !.


% Предикат для генерації слова заданої довжини, яке приймається автоматом
automata_word_length_k(K, Word) :-
    length(Word, K),                        % Генерація слова довжиною K
    maplist(symbol, Word),                  % Перевірка, чи всі символи належать алфавіту
    accept_word(Word).                      % Перевірка, чи приймається слово автоматом

% Приклад використання:
% find_x_and_xxx(9, 10). % Знаходження слів X та XXX довжиною від 9 до 10 символів

% Тестування
main :-
    writeln('Перевірка на мінімальну довжину'),
    \+ (find_x_and_xxx(0, 1) -> true ; false),
    writeln('Тест на мінімальну довжину успішно пройдено.'),
    writeln('Перевірка на велику довжину'),
    find_x_and_xxx(19, 20),
    writeln('Перевірка на випадкову довжину'),
    find_x_and_xxx(5, 8),
    writeln('Перевірка на неправильний діапазон'),
	\+ (find_x_and_xxx(3, 2) -> true ; false),
    writeln('Тест на неправильний діапазон успішно пройдено.').
