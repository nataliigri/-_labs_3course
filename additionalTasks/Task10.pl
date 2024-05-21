% Перевірка, чи елемент входить в список
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Пошук позицій входження елемента у список разом з індексами
positions(_, [], _, []).
positions(X, [X|T], Acc, [Acc|Positions]) :-
    NewAcc is Acc + 1,
    positions(X, T, NewAcc, Positions).
positions(X, [_|T], Acc, Positions) :-
    NewAcc is Acc + 1,
    positions(X, T, NewAcc, Positions).

% Знаходження позицій для унікальних елементів першого списку
find_positions([], _, _, []).
find_positions(_, [], _, []).
find_positions([H|T], List2, Seen, [Positions|Result]) :-
    \+ member(H, Seen),
    positions(H, List2, 0, Positions),
    find_positions(T, List2, [H|Seen], Result).
find_positions([H|T], List2, Seen, Result) :-
    member(H, Seen),
    find_positions(T, List2, Seen, Result).

% Основний виклик
find_positions(List1, List2, Result) :-
    (List1 = [] ; List2 = []) -> Result = []
    ; find_positions(List1, List2, [], Result).

% Приклад використання:
% ?- find_positions([1, 2, 3, 2, 1], [1, 2, 3, 1, 2, 3, 1], Positions).
% Positions = [[0, 3, 6], [1, 4], [2, 5]].
% ?- find_positions([1, 2, 3], [], Positions).
% Positions = [].
