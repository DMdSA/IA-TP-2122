%----------------------
% File written by------
% ---------------DMdSA-
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload


% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').
:- consult('texts.pl').
:- consult('Invariants.pl').


:- discontiguous q4/2 .
:- write("#> \"queries()\" to know what queries are available ..."),nl,nl.


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

validate_transp(bicycle(W,10,0)) :- 
			(W > 0 , W =< 5).

validate_transp(car(W, 25, 2)) :- 
			(W > 0, W =< 100).

validate_transp(motorcycle(W,35, 1)) :- 
			(W > 0, W =< 20).



/*
---------------------
IsAfter, confirms if a date is after another one
---------------------
*/

isAfter(date(_,_,Y),date(_,_,Y1)) :-
            (Y < Y1) , !.

isAfter(date(_,M,Y),date(_,M1,Y)) :-
            (M < M1) , !.

isAfter(date(D,M,Y), date(D1,M,Y)) :-
            (D =< D1).



/*
---------------------
Query1,  Estafeta que realizou (+) vezes um meio de transporte (+) ecológico
query1 : Estafeta, Meio, Count -> {V,F}

exemplos: 
query1(E, bicycle(_,_,_), C).
query1(E, car(_,_,_), C)

---------------------
*/

q1(Estafeta, Meio, Count) :-
                    agrupa_encomendas(Meio, List),
                    mymax((Estafeta, Count), List).



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

---------------------
*/  

/*
--------------------------------------------------------------------------outdated----
query2(Client, [Package], [Estafeta]) :-
                    
                    who_delivered(Client, Package, Estafeta).
                    
query2(Client, [H | T], [E1 | E2]) :-
                
                who_delivered(Client, H, E1),
                query2(Client, T, E2).



who_delivered(ClientID, PackageID, (EstafetaID, PackageID)) :-
                
                record(PackageID, ClientID, EstafetaID, _, _, _), !.

--------------------------------------------------------------------------outdated----
*/

% -----------------------------------------------------------------------
% Auxiliar functions
% Verify_Client confirms if an "ID" or "NAME" is referent to some client

% "rule here"
verify_client(X) :- client(X,_), ! ; client(_,X).
verify_client(client(X,Y)) :- client(X,Y).

% -----------------------------------------------------------------------

% ClientID, returns the Client ID associated with an "ID" or a "NAME"

% "rule here"
clientID(X, X) :- client(X,_), !.
clientID(X, Y) :- client(Y,X).
clientID(client(X,_), X).
% -----------------------------------------------------------------------



% ------------------------------------------------
% q2 : Client, [(Encomenda, Estafeta)] -> {V,F}   |
% ------------------------------------------------
% Dado um cliente, devolve a lista de (Encomenda, Estafeta) que lhe estão associados

% "rule here"
q2(Client, List) :- 
        
        verify_client(Client),
        clientID(Client, Aux),
        findall((Enc,Est), record(Enc, Aux, Est,_,_,_), List), !.
% ------------------------------------------------


% -------------------------------------------------------
% q2 : Client, Estafeta, [Encomendas] -> {V,F} |
% -------------------------------------------------------
% Dado um cliente e um estafeta, devolve uma lista com todas as encomendas que foram entregues àquele cliente, por aquele estafeta

% "rule here"
q2(Client, Estafeta, Encomendas) :-
        verify_client(Client),
        estafeta(Estafeta, _, _),
        clientID(Client, Aux),
        findall(Enc, record(Enc, Aux, Estafeta,_,_,_), Encomendas), !.
% -------------------------------------------------------

% ------------------------------------------------------------
% q2 : Client, [Encomendas], [(Encomenda, Estafeta)] -> {V,F} |
% ------------------------------------------------------------
% Dado um cliente e uma lista de encomendas, devolve uma lista de encomendas associadas ao respetivo estafeta da entrega

% "rule here"
q2(Client, Encomendas, List) :-
        
        verify_client(Client),
        clientID(Client, Aux),
        findall((Enc,Est), (record(Enc, Aux, Est,_,_,_), member(Enc, Encomendas)), List), !.
% -------------------------------------------------------





/*
---------------------
Query3, Identificar os clientes servidos por um determinado estafeta
---------------------
*/

% ------------------------------------
% ------------------------------------
% q3 : Estafeta, [Clients] -> {V,F} |
% ------------------------------------
% Dado um estafeta, devolve que clientes receberam encomendas desse mesmo estafeta

% "rule here"
q3(Estafeta, A) :-
                            
                            findall(X, record(_, X, Estafeta,_,_,_), L),
                            sort(L, A),
                            write_q3(Estafeta, A), !.
% --------------------------------------

% --------------------------------------
% q3 : Client, [Estafetas] -> {V,F} |
% --------------------------------------
% Dado um cliente, devolve a lista de estafetas que lhe fizeram entregas


% "rule here"
q3(Client, Answer) :-
            
            verify_client(Client),
            clientID(Client, Aux),
            findall(Est, record(_, Aux, Est,_,_,_), Estafetas),
            sort(Estafetas, Answer),!.
% --------------------------------------

% "Experimentar Client == Estafeta, como distinguir? se aparecerem os 2, se houvesse texto podia-se perceber"



% "Auxiliar function here"
write_q3(Estafeta, [H]):-
            estafeta(Estafeta, _,_),
            write("Client: "), write(H), write(" "),
            client(H,X), write(X),nl,nl,
            write("Estafeta: "), write(Estafeta),nl,!.

write_q3(E, [H|T]) :-
            estafeta(E,_,_),
            write("Client: "), write(H), write(" "),
            client(H,X), write(X), nl,
            write_q3(E,T).







/*
---------------------
Query4, Calcular o valor faturado num determinado dia
---------------------
*/


% ------------------------------------
% q4 : Date, Value -> {V,F}
% ------------------------------------
% Dada uma data, devolve o total de dinheiro que a empresa fez nesse dia

% "code here"
q4(date(D,M,Y), Value) :-
                
                date(D,M,Y), !,
                findall(V, package(_,_,_,V,_,date(D,M,Y)), Aux),
                sum_list(Aux, Value).


% ------------------------------------
% q4 : Month, Year, Value -> {V,F}    |
% ------------------------------------
% Dados um mês e ano, devolve o valor ganho nessa data

% "code here"
q4(M, Y, Value) :- 
                
                member(M, [1,2,3,4,5,6,7,8,9,10,11,12]),
                Y > 0,
                findall(V, package(_,_,_,V,_,date(_,M,Y)), Aux),
                sum_list(Aux, Value).


% ------------------------------------
% q4 : Year, Value -> {V,F}           |
% ------------------------------------
% Dado um ano, calcula o valor ganho nesse ano

% "code here"
q4(Y, Value) :- 

                Y > 0,
                findall(V, package(_,_,_,V,_,date(_,_,Y)), Aux),
                sum_list(Aux, Value).
% ------------------------------------




/*
---------------------
Query5, Identificar as zonas (rua, freguesia) com maior volume de entregas
por parte do Green Distribution
---------------------
*/

% ------------------------------------
% q5 : Address, NumberOfDeliveries -> {V,F}
% ------------------------------------
% Recebendo uma rua, freguesia, ou ambos, devolve o nº de entregas realizadas nessa localização

q5(address(Rua, Freguesia), Volume) :-

            findall(1, package(_,_,_,_,address(Rua,Freguesia),_), List),
            sum_list(List, Volume),
            address(Rua,Freguesia),!.

% "here"
% Realizar a query só para uma "rua" não fará muito sentido....
% Não há maneira de corrigir esse pormenor?
% "here"





/*
---------------------
Query6 - Calcular a classificação média de satisfação de cliente para
um determinado estafeta
---------------------
*/

% ---------------------------------------------------
% Average of list
% average : List, Average -> {V,F}
% ---------------------------------------------------

average( List, Average) :-
    sum_list(List, Sum),
    length( List, Length),
    Length > 0,
    Average is Sum/Length.

% "here", podia-se fazer if not, avisa que o cliente nao deu nota nenhuma
% ---------------------------------------------------

% ---------------------------------------------------
% q6 : Estafeta, Value -> {V,F}
% ---------------------------------------------------
% Dado um estafeta, calcula a média geral que os clientes lhe dão

q6(Estafeta, Value) :-
            
            estafeta(Estafeta, _, _),
            findall(X, record(_,_,Estafeta,_, _,X), L),!,
            average(L,Value).
% ---------------------------------------------------

% ---------------------------------------------------
% q6 : Client, Estafeta, Value -> {V,F}
% ---------------------------------------------------
% Dado um cliente e um estafeta, calcula a média que esse cliente atribuiu ao estafeta

q6(Client, Estafeta, Value) :-
                
                verify_client(Client),
                clientID(Client, ID),
                findall(X, record(_, ID, Estafeta, _, _,X), L),
                average(L, Value).
% ---------------------------------------------------




/*
---------------------------------------------------
Query7, Calcular o nº de entregas pelos diferentes meios de transporte, num intervalo de tempo.
query7 : Data, Data -> {V,F}

---------------------------------------------------
*/




q7(Date, Answer) :-
        
        Date,
        findall(ID, record(ID,_,_,Date,_,_), ListaPackage),
        findall(X, (member(P, ListaPackage), record(P,_,_,_,X,_)), List),
        agrupa(List, Answer),!,
        writeq7(Answer).

q7(Date1,Date2,Answer) :-

    (Date1, Date2),
    isAfter(Date1,Date2),
    filter_by_date_Packages(Date1, Date2, IDEncomendas),
    findall(X, (member(P, IDEncomendas), record(P,_,_,_,X,_)), List),
    agrupa(List, Answer),!, writeq7(Answer).


writeq7([]) :- nl.
writeq7([(H,NTimes) | Resto]) :-
        
        transport(X, H), write(" ["),
        write(NTimes), write("] package(s) was delivered by "), write(X), nl,
        writeq7(Resto).



/*
---------------------------------------------------
Query8, Calcular o n de entregas por estafeta, num intervalo de tempo
query8 : Data, Data -> {V,F}
---------------------------------------------------
*/ 

q8(Date1,Date2, Answer) :-
            (Date1, Date2),
            isAfter(Date1,Date2),
            filter_by_date_Packages(Date1, Date2, Encomendas),
            findall((Est), (member(Enc, Encomendas), record(Enc, _, Est, _, _, _)), EstDateAux),
            agrupa(EstDateAux,Answer),!,
            writeq8(Answer).


writeq8([]) :- nl.
writeq8([(H,NTimes) | Resto]) :-
    write("Courier ["), write(H), write("] delivered ["), write(NTimes), write("] packages."), nl,
     writeq8(Resto).


remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

agrupa([], []).
agrupa([X], [(X, 1)]).
agrupa([H | T], [(H, NTimes) | Resto]) :-
                findall(H, member(H, T), L),
                length(L, NTimesAux),
                NTimes is NTimesAux + 1,
                remover(H,T,T2),
                agrupa(T2, Resto).


filter_by_date_Packages(Date1, Date2, Package) :-
                    findall(C, (record(C,_,_,DATE,_,_),
                    dateInBetween(DATE, Date1, Date2)), Package).


dateInBetween(date(D,M,Y), date(D1,M1,Y1), date(D2,M2,Y2)) :-
        \+isAfter(date(D,M,Y),date(D1,M1,Y1)),
          isAfter(date(D,M,Y),date(D2,M2,Y2)).



/*
---------------------
Q10 - Calcular o PESO TOTAL transportado por UM estafeta NUM determinado dia
q10: Estafeta, Data, Weight -> {V,F}
Exemplo: q10(1, date(18,11,2021), P).
---------------------
*/ 

% ---------------------------------------------------
% q10 : EstafetaID, Date, Peso -> {V,F}
% ---------------------------------------------------
% 

q10(EstafetaID, Date, Peso) :-
        
                Date,
                estafeta(EstafetaID,_,_),
                findall(ID, record(ID,_,EstafetaID,Date,_,_), ListaPackage),
                findall(P,(member(ID, ListaPackage), package(ID, P,_,_,_,_)), ListaPeso),
                sum_list(ListaPeso, Peso),!.



% ---------------------------------------------------

% ---------------------------------------------------
q10(Date, Answer) :- 

        findall(E, estafeta(E,_,_), X), sort(X,A),
        q10aux(Date, A, Answer).


% ---------------------------------------------------

% ---------------------------------------------------
q10aux(_, [], []).
q10aux(Date, [H|T], [(H,P) | R]) :-
        
        q10(H, Date, P),
        q10aux(Date, T, R).
