% Предикат для генерації списку простих чисел за допомогою решета Ератосфена
prime_sieve(N, Primes) :-
    sieve(2, N, [], PrimesReversed),
    reverse(PrimesReversed, Primes).

sieve(N, N, Primes, Primes).
sieve(I, N, Acc, Primes) :-
    I =< N,
    (   is_prime(I)
    ->  Acc1 = [I | Acc],
        I1 is I + 1,
        sieve(I1, N, Acc1, Primes)
    ;   I1 is I + 1,
        sieve(I1, N, Acc, Primes)
    ).


is_prime(2).
is_prime(3).
is_prime(P) :-
    integer(P),
    P > 3,
    P mod 2 =\= 0,
    \+ has_factor(P, 3).

has_factor(N, L) :- N mod L =:= 0.
has_factor(N, L) :- L * L < N, L2 is L + 2, has_factor(N, L2).

% Предикат для розділення списку чисел за заданими простими числами
splitByPrimes([], _, []).
splitByPrimes(_, [], []).
splitByPrimes(InputList, [P|Primes], Result) :-
    partitionList(P, InputList, Smaller, Rest),
    (   Smaller = []
    ->  SmallerList = [[]]
    ;   SmallerList = [Smaller]
    ),
    append(SmallerList, RestList, Result),
    splitByPrimes(Rest, Primes, RestList).


% Допоміжний предикат для розділення списку на дві частини: одна з елементами менше P, інша - залишок
partitionList(_, [], [], []).
partitionList(P, [X|Xs], Smaller, Rest) :-
    X < P,
    partitionList(P, Xs, SmallerRest, Rest),
    Smaller = [X|SmallerRest].
partitionList(P, [X|Xs], Smaller, [X|Rest]) :-
    X >= P,
    partitionList(P, Xs, Smaller, Rest).


% Приклад використання:
task(InputList, Result) :-
    prime_sieve(30, Primes), % Генеруємо список простих чисел до 30
    splitByPrimes(InputList, Primes, Result).

main :-
    test(empty_input),
    test(single_number_input),
    test(input_list),
    test(prime_numbers_generation).

test(empty_input) :-
    task([], Result),
    (   Result = []
    ->  format('Тест empty_input успішно пройдено~n')
    ).

test(single_number_input) :-
    task([27], Result),
    (   Result = [[],[],[],[],[],[],[],[],[],[27]]
    ->  format('Тест single_number_input успішно пройдено~n')
    ).

test(input_list) :-
    task([20,20,13,1,18,19,5,14,16,3,12,3,2,9,6], Result),
    (   Result = [[1],[2],[3,3],[5,6],[9],[12],[13,14,16],[18],[20,20,19]]
    ->  format('Тест input_list успішно пройдено~n')
    ).

test(prime_numbers_generation) :-
    prime_sieve(20, Result),
    (   Result = [2, 3, 5, 7, 11, 13, 17, 19]
    ->  format('Тест prime_numbers_generation успішно пройдено~n')
    ).


% Приклад використання:
%?- task([29,19,31,31,47,19,35,10,6,22,15,20,41,7,21,9,30,14,1,47,46,45,2,38,14,31], Result).
