% Основний предикат для розпізнавання токенів з вхідного рядка
tokenize(Input, Tokens) :-
    string_chars(Input, Chars),
    tokenize_chars(Chars, Tokens).

% Перевірка кінця вводу
tokenize_chars([], []).

% Пропуск пробілів
tokenize_chars([' '|T], Tokens) :-
    tokenize_chars(T, Tokens).

% Пропуск інших пробільних символів (табуляції, нових рядків)
tokenize_chars([H|T], Tokens) :-
    char_type(H, space),
    tokenize_chars(T, Tokens).

% Розпізнавання чисел
tokenize_chars([H|T], [Token|Tokens]) :-
    char_type(H, digit),
    span(digit, [H|T], Digits, Rest),
    number_chars(Number, Digits),
    format(atom(Token), 'Number: ~w', [Number]),
    tokenize_chars(Rest, Tokens).

% Розпізнавання ідентифікаторів або службових слів
tokenize_chars([H|T], [Token|Tokens]) :-
    char_type(H, alpha),
    span(alnum, [H|T], Chars, Rest),
    atom_chars(Id, Chars),
    (   keyword(Id) -> format(atom(Token), 'Keyword: ~w', [Id])
    ;   format(atom(Token), 'Identifier: ~w', [Id])
    ),
    tokenize_chars(Rest, Tokens).

% Розпізнавання дволітерних операторів
tokenize_chars([H1,H2|T], [Token|Tokens]) :-
    two_char_operator([H1,H2]),
    atom_chars(Symbol, [H1,H2]),
    format(atom(Token), 'Double_operator: ~w', [Symbol]),
    tokenize_chars(T, Tokens).

% Розпізнавання однолітерних роздільників
tokenize_chars([H|T], [Token|Tokens]) :-
    one_char_separator(H),
    format(atom(Token), 'Symbol: ~w', [H]),
    tokenize_chars(T, Tokens).

% Розпізнавання однолітерних операторів
tokenize_chars([H|T], [Token|Tokens]) :-
    member(H, ['+', '-', '*', '/', '=', '<', '>', '!', '&', '|']),
    format(atom(Token), 'Single_operator: ~w', [H]),
    tokenize_chars(T, Tokens).

% Допоміжний предикат для розпізнавання послідовності символів
span(_, [], [], []).
span(Pred, [H|T], [H|R], Rest) :-
    call(Pred, H),
    span(Pred, T, R, Rest).
span(_, Rest, [], Rest).

% Предикати для перевірки типу символів
digit(H) :-
    char_type(H, digit).

alnum(H) :-
    char_type(H, alnum).

% Службові слова
keywords(['if', 'else', 'while', 'return', 'int']).

keyword(X) :-
    keywords(Keywords),
    member(X, Keywords).

% Однолітерні роздільники
one_char_separator('(').
one_char_separator(')').
one_char_separator('{').
one_char_separator('}').
one_char_separator('[').
one_char_separator(']').
one_char_separator(';').

% Дволітерні оператори
two_char_operator([=,=]).
two_char_operator([!,=]).
two_char_operator([<,=]).
two_char_operator([>,=]).
two_char_operator([<,<]).
two_char_operator([>,>]).
two_char_operator([<,<]).


% tokenize("+ - * / = < > ! & |", Tokens)
%tokenize("== != <= >= && ||", Tokens)
% tokenize("int main() { int a = 10; if (a >= 10) { a = a + 1; } return a; }", Tokens).
