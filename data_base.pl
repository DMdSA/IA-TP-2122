%----------------------
% File written by------
% ---------------DMdSA-
%----------------------

:- set_prolog_flag(encoding, utf8).

/*
    |dynamic predicate|
Informs the interpreter that the definition of the predicate(s)
may change during execution (using assert/1 and/or retract/1).
*/


:- dynamic transport/2 .
:- dynamic bicycle/3 .
:- dynamic motorcycle/3 .
:- dynamic car/3 .
:- dynamic address/2 .
:- dynamic client/2 .

:- discontiguous package/6 .
:- discontiguous record/6 .



/*
    | op(Precedence, Type, Name)

Declare "name" to be an operator. Precedence is an integer [0,1200]. Precedence = 0 removes the
declaration.
'y' -> on this position a term with precedence lower or equal to the precedence of the functor 
should occur.
'x' -> the precedence of the argument must be strictly lower.

*/   


transport('Bicycle', 1).
transport('Motorcycle', 2).
transport('Car', 3).

/*
---------------------
 bicycle
 bicycle : Weight , Average_speed, Ecological_value -> {V,F}

 motorcycle
 motorcycle : Weight , Average_speed, Ecological_value -> {V,F}

 car
 car : Weight, Average_speed, Ecological_value -> {V,F}
---------------------
*/


bicycle(5, 10, 0).
bicycle(3,10,0).

% Aqui digo que há 2 bicicletas, mas o objetivo não é dizer quantas, mas quais...
% 30 estafetas, 2 tipos bicicletas


motorcycle(20, 35, 1).
motorcycle(12, 35, 1).

car(100, 25,2).
car(32, 25,2).





/*---------------------
Package
package : Codigo, Peso, Volume, Valor, Morada, DataCriacao -> {V,F}

Record
record : PackageID, ClientID, EstafetaID, DeliverDate, TransportID, DeliverRate -> {V,F}
---------------------
*/


package(344053, 0.988, 20, 876, address('Rua Conego Manuel Faria', 'Sé'), date(17,11,2021)). 
record(344053, 2215, 1, date(18, 11, 2021),2, 4).

package(11234, 13, 102, 2.099, address('Rua wow', 'Gualtar'), date(11,11,2021)).
record(11234, 12345, 1, date(18, 11, 2021), 1, 5).

package(88341, 2.112, 122, 44.59, address('Rua ', 'Sé'), date(03, 10, 2021)).
record(88341, 12345, 1, date(03, 10, 2021), 1, 2).

package(6625, 88, 122, 443, address('Rua 2', 'Sé'), date(03, 10, 2021)).
record(6625, 12345, 4, date(04, 10, 2021), 3, 3).


package(5344, 88, 122, 200, address('Rua wow', 'Gualtar'), date(03, 10, 2021)).
record(5344, 2223, 4, date(18, 11, 2021), 3, 3).



/*
---------------------
Estafeta
estafeta : ID, Meio, [h | t], Nota-media (varia consoante penalizacoes)

[h|t] -> lista de package´s ID´s
confirmar a existencia do packet por packet(_,_,...) ou por record ?
---------------------
*/



estafeta(1, bicycle(5,10,0), [11234, 88341]).
estafeta(1, motorcycle(12,35,1), [344053]).
estafeta(3, car(32, 25,2), []).
estafeta(4, car(100, 25,2), [6625, 5344]).


%- Todos os package id existem na BD

validate_pkg_unicity([]).
validate_pkg_unicity([H | T]) :- 
                
                package(H,_,_,_,_,_),
                validate_pkg_unicity(T).

%- Todos os package id estao POR entregar

validate_to_deliver([]).
validate_to_deliver([H | T]) :- 
                
                \+record(H,_,_,_,_,_),
                validate_to_deliver(T).
            





/*---------------------
Date
date : Day, Month, Year -> {V,F}
---------------------
*/

date(D,M,A) :- D >= 1, D =< 30, member(M, [4,6,9,11]),
                A > 0, !.
date(D,M,A) :- D >= 1, D =< 31, member(M, [1,3,5,7,8,10,12]),
                A > 0, !.
date(D,2,A) :- D >= 1, D =< 29,
                A mod 4 =:= 0, A mod 100 =:= 0, A mod 400 =:= 0.
date(D,2,A) :- D >= 1, D =< 29,
                A mod 4 =:= 0, A mod 100 =\= 0 .
date(D,2,A) :- D >= 1, D =< 28,
                A mod 4 =:= 0, A mod 100 =:= 0, A mod 400 =\= 0.
date(D,2,A) :- D >= 1, D =< 28,
                A mod 4 =\= 0.

validate_date(date(D,M,Y)) :- date(D,M,Y).


/*
---------------------
Client
client : NIF, Nome -> {V,F}
---------------------
*/

client(12345, 'Diogo Araújo').
client(88321, 'Joel Araújo').
client(02223, 'Mateus Coelho').
client(2215, 'Diogo Rebelo').


/*
---------------------
Address
address : Rua, Freguesia -> {V,F}
---------------------
*/


address('Rua Conego Manuel Faria', 'Sé').
address('Rua 2', 'Sé').
address('Rua 3', 'Sé').
address('Rua wow', 'Gualtar').

validate_address(address(R, F)) :-
    atom(R), atom(F), address(R,F).