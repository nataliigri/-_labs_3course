% Перевірка, чи елемент є членом списку
member(X, [X | _]).
member(X, [_ | Tail]) :- member(X, Tail).

% Визначення кількості різних елементів у списку
count_unique([], 0).
count_unique([Head | Tail], Count) :-
    member(Head, Tail), !, % Виключаємо повторення
    count_unique(Tail, Count).
count_unique([_ | Tail], Count) :-
    count_unique(Tail, SubCount),
    Count is SubCount + 1.

% Перевірка одного тесту
run_test(Test, TestName) :-
    call(Test),
    write('Тест '), write(TestName), write(' пройдений.'), nl.

% Запуск всіх тестів і виведення результатів
run_tests :-
    run_test(count_unique([], 0), '1: порожній список'),
    run_test(count_unique([1], 1), '2: список з одним елементом'),
    run_test(count_unique([1, 2, 3, 1, 2, 4, 5, 3, 6], 6), '3: список з дубльованими елементами'),
    run_test(count_unique([1, 2, 3, 4, 5, 6], 6), '4: список з усіма елементами різними'),
    write('Всі тести пройдені.').
