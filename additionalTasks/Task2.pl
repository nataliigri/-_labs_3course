% Предикат для переведення числа в двійковий запис
to_binary(0, []).
to_binary(N, [Bit | Bits]) :-
    N > 0,
    Bit is N mod 2,
    Next is N // 2,
    to_binary(Next, Bits).

% Предикат для знаходження передостанньої цифри в двійковому записі
find_second_last_digit(N, SecondLast) :-
    to_binary(N, [_, SecondLast | _]).

% Приклад використання:
% ?- find_second_last_digit(18, Digit).

% Тест 1: Перевірка для числа 10 (1010 в двійковій)
test_find_second_last_digit_1 :-
    find_second_last_digit(10, 1). % Очікуваний результат: 1

% Тест 2: Перевірка для числа 27 (11011 в двійковій)
test_find_second_last_digit_2 :-
    find_second_last_digit(27, 1). % Очікуваний результат: 1

% Тест 3: Перевірка для числа 8 (1000 в двійковій)
test_find_second_last_digit_3 :-
    find_second_last_digit(8, 0). % Очікуваний результат: 0

% Тест 4: Перевірка для числа 15 (1111 в двійковій)
test_find_second_last_digit_4 :-
    find_second_last_digit(15, 1). % Очікуваний результат: 1

% Функція для виконання всіх тестів
main :-
    test_find_second_last_digit_1,
    test_find_second_last_digit_2,
    test_find_second_last_digit_3,
    test_find_second_last_digit_4,
    writeln('All tests passed.').
