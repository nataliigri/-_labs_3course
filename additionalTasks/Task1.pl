% Визначення предикату, що знаходить унікальні елементи у списку
unique_elements([], []).
unique_elements([H | T], Unique) :-
    member(H, T),
    unique_elements(T, Unique).
unique_elements([H | T], [H | Unique]) :-
    \+ member(H, T),
    unique_elements(T, Unique).

% Підрахунок кількості унікальних елементів у списку
count_unique_elements(List, Count) :-
    unique_elements(List, Unique),
    length(Unique, Count).

% Перевірка одного тесту
run_test(Test, TestName) :-
    call(Test),
    write('Тест '), write(TestName), write(' пройдений.'), nl.

% Запуск всіх тестів і виведення результатів
run_tests :-
    run_test(count_unique_elements([], 0), '1: порожній список'),
    run_test(count_unique_elements([1], 1), '2: список з одним елементом'),
    run_test(count_unique_elements([1, 2, 3, 1, 2, 4, 5, 3, 6], 6), '3: список з дубльованими елементами'),
    run_test(count_unique_elements([1, 2, 3, 4, 5, 6], 6), '4: список з усіма елементами різними'),
    write('Всі тести пройдені.').
