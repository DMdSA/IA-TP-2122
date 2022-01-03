
:- dynamic validate_transp/1 .
:- dynamic isAfter/2 .
:- dynamic total_price/2 .
:- dynamic max_couple/2 .
:- dynamic verify_client/1 .
:- dynamic clientID/2 .
:- dynamic verify_possible_weight/2 .


/*
---------------------
validate_transp, verifica se um transporte é válido.
---------------------
*/

validate_transp(transport('Bicycle',W,10,0.7,0)) :- 
        (W > 0 , W =< 5).

validate_transp(transport('Car',W, 25, 0.1, 2)) :- 
        (W > 0, W =< 100).

validate_transp(transport('Motorcycle',W,35, 0.5, 1)) :- 
        (W > 0, W =< 20).



/*
---------------------
isAfter, verifica se uma data é posterior a outra.
---------------------
*/

isAfter(date(_,_,Y,_),date(_,_,Y1,_)) :-
        (Y < Y1) , !.

isAfter(date(_,M,_,_),date(_,M1,_,_)) :-
        (M < M1) , !.

isAfter(date(D,_,_,_), date(D1,_,_,_)) :-
        (D < D1) , !.

isAfter(date(_,_,_,H),date(_,_,_,H1)) :-
        (H =< H1).



/*
---------------------
total_price, calcula o preço total de um package.
---------------------
*/

total_price(package(C,W,V,P,_,_,H), X) :-
                
        record(C,_,_,_,TName,_),
        transport(TName,_,_,_,ECO),!,
        Y is div(5,5*H+1),
        X is 2.75 + P + 0.2*W + 0.15*V + 2*ECO + Y.



/*
---------------------
max_couple, 
---------------------
*/

max_couple([(Est,NEnc)], (Est,NEnc)).
max_couple([(Est,NEnc) | R], (Est,NEnc)) :- 
        
        max_couple(R, (_,Aux)),
        NEnc > Aux.

max_couple([(_,NEnc) | R], (A,B)) :- 
        
        max_couple(R, (A,B)),
        B >= NEnc.



/*
---------------------
verify_client, verifica~se determinado id ou nome estão na base de dados dos clientes.
---------------------
*/

verify_client(X) :- client(X,_), ! ; client(_,X).
verify_client(client(X,Y)) :- client(X,Y).



/*
---------------------
clientID, retorna o ID do cliente associado ao nome ou id dado.
---------------------
*/

clientID(X, X) :- client(X,_), !.
clientID(X, Y) :- client(Y,X).
clientID(client(X,_), X).



/*
---------------------
average, calcula a media entre os valores de uma lista.
---------------------
*/

average( List, Average) :-
        sum_list(List, Sum),
        length( List, Length),
        Length > 0,
        Average is Sum/Length.



/*
----------------------
remover, remove todas as ocurrências de um determinado elemnto numa lista.
----------------------
*/

remover( _, [], []).
remover( R, [R|T], T2) :-
        remover( R, T, T2).
remover( R, [H|T], [H|T2]) :-
        H \= R, remover( R, T, T2).



/*
----------------------
agrupa, agrupa, numa lista, pares de elementos e o numero de vezes que estes ocorrem numa dadas lista.
----------------------
*/

agrupa([], []).
agrupa([X], [(X, 1)]).
agrupa([H | T], [(H, NTimes) | Resto]) :-
        findall(H, member(H, T), L),
        length(L, NTimesAux),
        NTimes is NTimesAux + 1,
        remover(H,T,T2),
        agrupa(T2, Resto).



/*
----------------------
filter_by_date_estafetas, filtra os estafetas que entregaram encomendas num determinado periodo de tempo, e coloca-os numa lista.
----------------------
*/

filter_by_date_estafetas(Date1, Date2, EstafetasList) :-
        findall(C, (record(_,_,C,DATE,_,_),
        dateInBetween(DATE, Date1, Date2)), EstafetasList).



/*
----------------------
dateInBetween, verifica se uma data esta localizada entre 2 outras datas dadas.
----------------------
*/

dateInBetween(date(D,M,Y,H), date(D1,M1,Y1,H1), date(D2,M2,Y2,H2)) :-
        \+isAfter(date(D,M,Y,H),date(D1,M1,Y1,H1)),
          isAfter(date(D,M,Y,H1),date(D2,M2,Y2,H2)).



/*
---------------------
filter_by_date_Transport, filtra os transportes usados para entregar encomendas num determinado periodo de tempo, e coloca-os numa lista.
---------------------
*/

filter_by_date_Transport(Date1, Date2, Transport) :-
        findall(C, (record(_,_,_,DATE,C,_),
        dateInBetween(DATE, Date1, Date2)), Transport).



/*
---------------------
verify_possible_weight, verifica se é possivel transportar determinado peso.
---------------------
*/

verify_possible_weight([PkgW], W) :- PkgW =< W.
verify_possible_weight([H|T], W) :-                        
        H =< W,
        verify_possible_weight(T,W).