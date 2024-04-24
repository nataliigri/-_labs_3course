:- use_module(library(lists)).
:- use_module(library(random)).

% Функція для генерації простих чисел за допомогою решета Ератосфена
sieve([], []).
sieve([X|Xs], [X|Primes]) :-
    sieve(Xs, XsPrimes),
    exclude(multiple_of(X), XsPrimes, Primes).

multiple_of(X, Y) :- Y mod X =:= 0.

% Генеруємо нескінченний список простих чисел
primes(Primes) :- sieve([2,3,5,7,11,13,17,19,23,29,31], Primes).

% Функція, яка розбиває список за заданими простими числами
split_by_primes([], _, []).
split_by_primes(_, [], []).
split_by_primes([P|Ps], Xs, [Smaller|Rest]) :-
    partition(>(P), Xs, Smaller, Remaining),
    split_by_primes(Ps, Remaining, Rest).

% Головна функція для демонстрації
main :-
    N = 26,
    NumPrimes = 9, % Число простих чисел для розбиття списку
    RangeMin = 1,
    RangeMax = 50,
    length(RangeList, N),
    maplist(random_between(RangeMin, RangeMax), RangeList),
    writeln('Початковий список:'),
    writeln(RangeList),
    primes(AllPrimes),
    take(NumPrimes, AllPrimes, SelectedPrimes),
    split_by_primes(SelectedPrimes, RangeList, Result),
    writeln('Список після розбиття за простими числами:'),
    writeln(Result).

% Предикат для вибору перших N елементів списку
take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [X|Xs], [X|Rest]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Xs, Rest).

:- initialization(main).
