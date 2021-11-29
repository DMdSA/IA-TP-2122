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

package(1000000, 2.5, 25 , 30 , address('Rua Baldeira','Adaúfe'), date(11, 11, 2021)).
record(1000000, 10000, 1, date(11, 11, 2021), 1, 5).

package(1000001, 3.3, 27, 32, address('Rua Ortigueira', 'Adaúfe'), date(11, 11, 2021)).
record(1000001, 10001, 1, date(11, 11, 2021), 1, 4). 

package(1000002, 3.4, 20, 33, address('Rua Aida Gonçalves', 'Arentim'), date(11, 11, 2021)).
record(1000001, 10002, 1, date(11, 11, 2021), 1, 3). 

package(1000003, 5, 10, 15, address('Rua Aida Gonçalves', 'Arentim'), date(12, 11, 2021)).
record(1000003, 10002, 1, date(13, 11, 2021), 2, 1). 

package(1000004, 1, 5, 10, adress('Rua dos Regos', 'Cabreiros'), date(13, 11, 2021)).
record(1000004, 10003, 3, date(15, 11, 2021), 3, 3).



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


estafeta(1, bicycle(5,10,0), [1000000, 1000001, 1000002]).
estafeta(1, motorcycle(12,35,1), [1000003]).
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

client(10000, 'Diogo Araújo').
client(10001, 'Joel Araújo').
client(10002, 'Mateus Coelho').
client(10003, 'Diogo Rebelo').
client(10004, 'Miguel Silva').
client(10005, 'Afonso Faria').
client(10006, 'André Nunes').
client(10007, 'Duarte Mendes').
client(10008, 'Fábio Silva').
client(10009, 'Fernando Lopes').
client(10010, 'Filipe Oliveira').
client(10011, 'Gonçalo Afonso').
client(10012, 'Gonçalo Carvalaho').
client(10013, 'Gonçalo Pereira').
client(10014, 'Henrique Ribeiro').
client(10015, 'Isabel Perez').
client(10016, 'Jéssica Fernandes').
client(10017, 'José Duarte').
client(10018, 'José Peixoto').
client(10019, 'Maria Gomes').
client(10020, 'Mariem Khammassi').
client(10021, 'Miguel Martins').
client(10022, 'Nuno Oliveira').
client(10023, 'Pedro Alves').
client(10024, 'Raúl Parente').
client(10025, 'Rui Morais').
client(10026, 'Sara Silva').
client(10027, 'Simão Cunha').
client(10028, 'Tiago Silva').


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
address('Rua Balbeira', 'Adaúfe').
address('Rua Ortigueira', 'Adaúfe').
address('Rua Aida Gonçalves', 'Arentim').
address('Rua da Barroca', 'Arentim').
address('Rua da Sacota', 'Cabreiros').
address('Rua dos Regos', 'Cabreiros').
address('Rua da Portagem', 'Celeiros').
address('Rua da Quinta', 'Celeiros').
address('Rua da Escola', 'Crespos').
address('Rua da Gregossa', 'Crespos').
address('Rua da Encosta', 'Escudeiros').
address('Rua da Formiga', 'Escudeiros').
address('Rua das Carvalheiras', 'Espinho').
address('Rua das Fontaínhas', 'Espinho').
address('Rua das Vinhas', 'Esporões').
address('Rua do Carvalhal', 'Esporões').
address('Rua Cruz do Outeiro', 'São Mamede').
address('Rua da Batoca', 'São Pedro').
address('Rua da Esperança', 'Ferreiros').
address('Rua da Figueira', 'Ferreiros').
address('Rua das Mimosas', 'Figueiredo').
address('Rua de Coruche', 'Figueiredo').
address('Rua Monte de Baixo', 'Gualtar').
address('Rua da Pedreira', 'Gualtar').
address('Rua da Igreja Velha', 'Guisande').
address('Rua da Lage', 'Guisande').
address('Rua da Cabraínha', 'Lamas').
address('Rua da Mamoa', 'Lamas').
address('Rua da Varziela', 'Lomar').
address('Rua da Quinta das Mouras', 'Lomar').
address('Rua da Estação', 'Maximinos').
address('Rua das Portas', 'Maximinos').
address('Rua das Lavandeiras', 'Tibães').
address('Rua do Sobrado', 'Tibães').
address('Rua de Almoinha', 'Morreira').
address('Rua do Monte', 'Morreira').
address('Rua da Senra', 'Nogueira').
address('Rua das Flores', 'Nogueira').
address('Rua da Mina', 'Palmeira').
address('Rua da Paz', 'Palmeira').
address('Rua do Rio', 'Pedralva').
address('Rua do Souto', 'Pedralva').
address('Rua da Casa Nova', 'Priscos').
address('Rua do Castro', 'Priscos').
address('Rua da Cachada', 'Semelhe').
address('Rua da Escola', 'Semelhe').
address('Rua do Eiteiro', 'Ruilhe').
address('Rua do Ferreiro', 'Ruilhe').
address('Rua da Arcela', 'São Vitor').
address('Rua da Chamadeira', 'São Vitor').
address('Rua das Pochinhas', 'São Vicente').
address('Rua de Espanha', 'São Vicente').
address('Rua da Tomada', 'Sequeira').
address('Rua da Venda', 'Sequeira').

validate_address(address(R, F)) :-
    atom(R), atom(F), address(R,F).