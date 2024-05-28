place:- 
    write('Експертна система для пошуку ідеального місця для відпочинку на літеру В!'), nl,
    write('Але спершу дай відповідь на декілька запитань...'), nl, nl,
    place(X),
    write('Твоє ідеальне місце для відпочинку - '), 
    write(X),
    write('!').
place:- 
    write('На жаль, ми нічого не змогли підібрати... Спробуй ще раз!').

:- dynamic yes/2. % Динамічний предикат для збереження відповідей yes

% Питання, яке визначає, чи є задана атрибут-значення істинним
question(Attribute, Value):-
    (yes(Attribute, Value) -> true ; (
        write(Value), write('? (yes/no) '),
        read(Response),
        process_response(Response, Attribute, Value)
    )).

process_response(yes, Attribute, Value) :-
    asserta(yes(Attribute, Value)).
process_response(no, Attribute, Value) :-
    retract(yes(Attribute, Value)), !. % Remove the option from the list of possibilities

%Запитання з варіантами відповідей
questionWithPossibilities(Attribute, Value, Possibilities) :-
 write('Що Вам більше до душі'), write(Attribute), write('?'), nl,
 write(Possibilities), nl,
 read(X),
 check_val(X, Attribute, Value, Possibilities),
 asserta( retract(yes, Attribute, X) ),
 X == Value.

check_val(X, _, _, Possibilities) :-  member(X, Possibilities),
 !.
check_val(X, Attribute, Value, Possibilities) :-
 write(X), write(' is not a legal value, try again.'), nl,
 questionWithPossibilities(Attribute, Value, Possibilities). 

%retract equips this system with a memory that remembers the facts that are already
%known because they were already entered by the user at some point during the interaction.
:- dynamic(retract/3).

% Визначення запитів
historical(X) :- question('історичні пам\'ятки', X).
cuisine(X) :- question('їжа', X).
size(X) :- question('розмір місця', X).
country(X) :- question('країна', X).
features(X) :- question('популярний', X).
atmosphere(X) :- questionWithPossibilities('', X, ['активний відпочинок', 'дуже спокійна і класна атмосфера', 'пляж, море і сонечко']). 


% Визначення місць
place('Версаль') :-
    type(city),
    country('Франція'),
    cuisine('кава з круасаном'),
    historical('палац колишня резиденція королів').

place('Венеція') :- 
    type(city),
    country('Італія'),
    features('хотіли б поплавати на гондолі'),
    cuisine('піца або паста Карбонара').

place('Ворохта') :- 
    type(mountains), 
    country('Україна'),
    atmosphere('дуже спокійна і класна атмосфера'),
    features('неймовірні карпатські пейзажі'),
    cuisine('український борщ').

place('Відень') :- 
    type(city), 
    country('Австрія'),
    features('різдвяні ярмарки').

place('Валенсія') :- 
    type(sea), 
    country('Іспанія'),
    cuisine('паелья').

place('Вегас') :- 
    type(city), 
    country('США'),
    features('казино').

place('Верховина') :- 
    type(mountains), 
    country('Україна'),
    features('активний відпочинок').

place('Ватикан') :- 
    type(city), 
    country('Італія'),
    historical('хочете познайомитися з Папою Римським?'), 
    features('старовинна архітектура').


% Визначення типів місць
type(mountains) :- 
    features('красиві гірські краєвиди').

type(sea) :- 
    features('екзотичні пляжі').

type(city) :- 
    size('дуже велике місто'),
    features('багато відомих туристичних місць').


% Приклад використання
% ?- place(X).
% X = 'Версаль' ;
% X = 'Венеція' ;
% X = 'Ворохта' ;
% X = 'Відень' ;
% X = 'Валенсія' ;
% X = 'Вегас' ;
% X = 'Верховина' ;
% X = 'Ватикан'.
