% Знаходимо мінімальний та максимальний елементи в списку
min_max([], Min, Max, Min, Max).
min_max([H|T], CurrMin, CurrMax, Min, Max) :-
    NewMin is min(H, CurrMin),
    NewMax is max(H, CurrMax),
    min_max(T, NewMin, NewMax, Min, Max).

min_max([H|T], Min, Max) :-
    min_max(T, H, H, Min, Max).

% Видалення першого входження елемента зі списку
delete_first([], _, []).
delete_first([X|T], X, T).
delete_first([H|T], X, [H|Rest]) :-
    H \= X,
    delete_first(T, X, Rest).

% Переміщуємо мінімальний та максимальний елементи списку так, щоб найменший був на початку, а найбільший в кінці
move_elements(List, Result) :-
    min_max(List, Min, Max),
    delete_first(List, Min, Temp1),
    delete_first(Temp1, Max, Middle),
    append([Min], Middle, Temp2),
    append(Temp2, [Max], Result).

% Основний предикат для перестановки елементів
rearrange_elements(List, Result) :-
    min_max(List, Min, Max),
    move_elements(List,Result).

/** <examples>
?- ?- rearrange_elements([10, 9, 8, 7, 10], Result).

?- rearrange_elements([10, 9, 8, 7, 10], Result).
*/

% Предикат для тестування функції min_max
test_min_max :-
    % Тести для мінімального та максимального елементів
    min_max([3, 5, 2, 8, 1], Min1, Max1),
    min_max([-10, 0, 15, 7, -5], Min2, Max2),
    % Перевірка на очікувані результати
    (   Min1 =:= 1, Max1 =:= 8 ->
        writeln('Тест 1 пройдений успішно: мінімальні та максимальні елементи вірні.')
    ;   writeln('Тести 1 не пройдений: мінімальний або максимальний елементи не вірні.')
    ), 
    % Перевірка на очікувані результати
    (   Min2 =:= -10, Max2 =:= 15 ->
        writeln('Тест 2 пройдений успішно: мінімальні та максимальні елементи вірні.')
    ;   writeln('Тест 2 не пройдений: мінімальний або максимальний елементи не вірні.')
    ).

% Предикат для тестування функції move_elements
test_move_elements :-
    % Тест зі списком [3, 5, 2, 8, 1]
    move_elements([3, 5, 2, 8, 1], Result1),
    % Перевірка на очікувані результати
    (   Result1 = [1, 3, 5, 2, 8] ->
        writeln('Тест 3 пройдений успішно: елементи вірно переміщені.')
    ;   writeln('Тест 3 не пройдений: елементи не вірно переміщені.')
    ),
    % Тест зі списком [-10, 0, 15, 7, -5]
    move_elements([-10, 0, 15, 7, -5], Result2),
    % Перевірка на очікувані результати
    (   Result2 = [-10, 0, 7, -5, 15] ->
        writeln('Тест 4 пройдений успішно: елементи вірно переміщені.')
    ;   writeln('Тест 4 не пройдений: елементи не вірно переміщені.')
    ).

% Предикат для виконання тестів
run_tests :-
    % Виклик тесту для мінімального та максимального елементів
    test_min_max,
    % Перевірка результатів тестування min_max
    (   \+ \+ current_prolog_flag(test_failure, false) ->
        writeln('min_max Тести не пройдені.')
    ;   writeln('min_max Всі тести пройдені успішно.')
    ),
    % Виклик тесту для функції move_elements
    test_move_elements,
    % Перевірка результатів тестування move_elements
    (   \+ \+ current_prolog_flag(test_failure, false) ->
        writeln('move_elements Тести не пройдені.')
    ;   writeln('move_elements Всі тести пройдені успішно.')
    ).
