%----------------------
% File written by------
% ---------------DMdSA-
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload




:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').





/*
---------------------
 Ecological aspect
		0-1-2
 0 -> bicycle
 1 -> car
 2 -> motorcycle
---------------------
*/



/*
---------------------
 Validation of a transport
 validate(Transport).
 This only confirms if a specific transport mean is accepted
---------------------
*/

validate(bicycle(W,10,0)) :- 
			(W > 0 , W =< 5).

validate(car(W, 25, 1)) :- 
			(W > 0, W =< 100).

validate(motorcycle(W,35, 2)) :- 
			(W > 0, W =< 20).






%---------------------
% Generate a package price
%---------------------






/*
---------------------
Query1,  Estafeta que realizou (+) vezes um meio de transporte (+) ecológico
query1 : Estafeta, Meio, Count -> {V,F}

exemplos: 
query1(E, bicycle(_,_,_), C).
query1(E, car(_,_,_), C)

---------------------
*/

query1(Estafeta, Meio, Count) :-
                    agrupa_encomendas(Meio, List),
                    mymax((Estafeta, Count), List),
                    !.



% Agrupa todos os estafetas que utilizam determinado meio, com o respetivo número de encomendas
% registadas na sua lista de entregas



mymax((Estafeta, NEncomendas), [(Estafeta,NEncomendas)]).
mymax((A, B), [(A,B) | Resto]) :-
                        
                        mymax((_,Aux), Resto),
                        B > Aux.

mymax((Est,NE), [(_,B) | Resto]) :-

                        mymax((Est,NE), Resto),
                        NE >= B.



agrupa_encomendas(Meio, Encomendas) :-
                    
                    findall((Est, List), estafeta(Est, Meio, List), L),
                    agrupa_n_encomendas(L, Encomendas).



% Recebe pares de (Estafeta, [Encomendas]) e devolve (Estafeta, NEncomendas)

agrupa_n_encomendas([(A,B)], [(A,C)]) :- length(B,C), !.
agrupa_n_encomendas( [(A,B) | Resto], [(A,C) | Wow]) :-
                        
                        length(B,C),
                        agrupa_n_encomendas(Resto, Wow).



/*
---------------------
Query2,  Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente
query2 : Client, Packages, Estafetas -> {V,F}

exemplos:
query2(12345,[11234, 88341, 6625],E).

---------------------
*/

query2(Client, [Package], [Estafeta]) :-
                    
                    who_delivered(Client, Package, Estafeta).
                    
query2(Client, [H | T], [E1 | E2]) :-
                
                who_delivered(Client, H, E1),
                query2(Client, T, E2).



who_delivered(ClientID, PackageID, (EstafetaID, PackageID)) :-
                
                record(PackageID, ClientID, EstafetaID), !.