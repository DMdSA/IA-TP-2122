%----------------------
% File written by------
% ---------------DMdSA-
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload


% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').
:- consult('Auxiliares.pl').
:- consult('Invariants.pl').
:- consult('texts.pl').

:- discontiguous q4/2 .

:- write("#> \"queries()\" to know what queries are available ..."),nl,nl.




/*
---------------------
Query1,  Estafeta que realizou (+) vezes um meio de transporte (+) ecológico
q1 : Estafeta, Meio, Count -> {V,F}

exemplos: 
---------------------
*/

q1(ID, Meio, Answer) :- 
        
        findall((ID,N), (estafeta(ID,Meio,Pkgs), length(Pkgs,N)),X),
        max_couple(X, Answer).


/*
---------------------
Query2,  Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente
---------------------
*/  

% ------------------------------------------------
% q2 : Client, [(Encomenda, Estafeta)] -> {V,F}   |
% ------------------------------------------------
% Dado um cliente, devolve a lista de (Encomenda, Estafeta) que lhe estão associados

% "rule here"
q2(Client, List) :- 
        
        verify_client(Client),
        clientID(Client, Aux),
        findall((Enc,Est), record(Enc, Aux, Est,_,_,_), List).


% -------------------------------------------------------
% q2 : Client, Estafeta, [Encomendas] -> {V,F} |
% -------------------------------------------------------
% Dado um cliente e um estafeta, devolve uma lista com todas as encomendas que foram entregues àquele cliente, por aquele estafeta

% "rule here"
q2(Client, Estafeta, Encomendas) :-
        verify_client(Client),
        estafeta(Estafeta, _, _),
        clientID(Client, Aux),
        findall(Enc, record(Enc, Aux, Estafeta,_,_,_), Encomendas).
% -------------------------------------------------------

% ------------------------------------------------------------
% q2 : Client, [Encomendas], [(Encomenda, Estafeta)] -> {V,F} |
% ------------------------------------------------------------
% Dado um cliente e uma lista de encomendas, devolve uma lista de encomendas associadas ao respetivo estafeta da entrega

% "rule here"
q2(Client, Encomendas, List) :-
        
        verify_client(Client),
        clientID(Client, Aux),
        findall((Enc,Est), (record(Enc, Aux, Est,_,_,_), member(Enc, Encomendas)), List) .


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
% q3 : Client, [Estafetas] -> {V,F} |
% --------------------------------------
% Dado um cliente, devolve a lista de estafetas que lhe fizeram entregas


% "rule here"
q3(Client, Answer) :-
            
            verify_client(Client),
            clientID(Client, Aux),
            findall(Est, record(_, Aux, Est,_,_,_), Estafetas),
            sort(Estafetas, Answer),!.


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
q4(date(D,M,Y,_), Value) :-
                
                date(D,M,Y,_), !,
                findall(V, package(_,_,_,V,_,date(D,M,Y,_)), Aux,_),
                sum_list(Aux, Value).


% ------------------------------------
% q4 : Month, Year, Value -> {V,F}    |
% ------------------------------------
% Dados um mês e ano, devolve o valor ganho nessa data

% "code here"
q4(M, Y, Value) :- 
                
                member(M, [1,2,3,4,5,6,7,8,9,10,11,12]),
                Y > 0,
                findall(V, package(_,_,_,V,_,date(_,M,Y,_)), Aux,_),
                sum_list(Aux, Value).


% ------------------------------------
% q4 : Year, Value -> {V,F}           |
% ------------------------------------
% Dado um ano, calcula o valor ganho nesse ano

% "code here"
q4(Y, Value) :- 

                Y > 0,
                findall(V, package(_,_,_,V,_,date(_,_,Y,_)), Aux,_),
                sum_list(Aux, Value).



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

            findall(1, package(_,_,_,_,address(Rua,Freguesia),_), List,_),
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
% q6 : Estafeta, Value -> {V,F}
% ---------------------------------------------------
% Dado um estafeta, calcula a média geral que os clientes lhe dão

q6(Estafeta, Value) :-
            
            estafeta(Estafeta, _, _),
            findall(X, record(_,_,Estafeta,_, _,X), L),!,
            average(L,Value).


% ---------------------------------------------------
% q6 : Client, Estafeta, Value -> {V,F}
% ---------------------------------------------------
% Dado um cliente e um estafeta, calcula a média que esse cliente atribuiu ao estafeta

q6(Client, Estafeta, Value) :-
                
                verify_client(Client),
                clientID(Client, ID),
                findall(X, record(_, ID, Estafeta, _, _,X), L),
                average(L, Value).




/*
---------------------------------------------------
Query7, Calcular o nº de entregas pelos diferentes meios de transporte, num intervalo de tempo.
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
    filter_by_date_Transport(Date1, Date2, Transport),
    agrupa(Transport, Answer),!, writeq7(Answer).


% "AUXILIAR TEXT HERE"
writeq7([]) :- nl.
writeq7([(H,NTimes) | Resto]) :-
        
        transport(X, H), write(" ["),
        write(NTimes), write("] package(s) was delivered by "), write(X), nl,
        writeq7(Resto).


% "COMMENT + OTHER FILE"
filter_by_date_Transport(Date1, Date2, Transport) :-
                    findall(C, (record(_,_,_,DATE,C,_),
                    dateInBetween(DATE, Date1, Date2)), Transport).


/*
---------------------------------------------------
Query8, Calcular o n de entregas por estafeta, num intervalo de tempo
query8 : Data, Data -> {V,F}
---------------------------------------------------
*/ 

q8(Date,Answer) :-
    Date,
    findall(ID, record(_,_,ID,Date,_,_), ListaEst),
    agrupa(ListaEst,Answer),!,
    writeq8(Answer).


q8(Date1,Date2, Answer) :-
            (Date1, Date2),
            isAfter(Date1,Date2),
            filter_by_date_estafetas(Date1, Date2, EstafetasList),
            agrupa(EstafetasList,Answer),!,
            writeq8(Answer).


% "AUXILIAR TEXT HERE"
writeq8([]) :- nl.
writeq8([(H,NTimes) | Resto]) :-
    write("Courier ["), write(H), write("] delivered ["), write(NTimes), write("] packages."), nl,
     writeq8(Resto).


% "AUXILIAR RULES, ADD TO OTHER FILE + COMMENT"
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


filter_by_date_estafetas(Date1, Date2, Package) :-
                    findall(C, (record(_,_,C,DATE,_,_),
                    dateInBetween(DATE, Date1, Date2)), Package).


dateInBetween(date(D,M,Y,H), date(D1,M1,Y1,H1), date(D2,M2,Y2,H2)) :-
        \+isAfter(date(D,M,Y,H),date(D1,M1,Y1,H1)),
          isAfter(date(D,M,Y,H1),date(D2,M2,Y2,H2)).

/*
---------------------------------------------------
Query9, calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo
query9 : Data, Data -> {V,F}
---------------------------------------------------
*/ 

q9(Date1, Date2, Total, Entregues, NEntregues):- 
        (Date1, Date2),
        isAfter(Date1,Date2),
        filter_by_date_encomendas(Date1, Date2, Encomendas),
        length(Encomendas,Total),
        findall(E, 
                        (
                        member(E, Encomendas),
                        record(_,_,_,DE,_,_),
                        data_esperada(E,DP),
                        isAfter(DP,DE)
                        )
                ),
                EncomendasConcluidas),
        length(EncomendasConcluidas, Entregues),
        NEntregues = Total - Entregues.




filter_by_date_encomendas(Date1, Date2, Package) :-
        findall(E, ( package(E,_,_,_,_,_,_), data_esperada(E,DATE), dateInBetween(DATE, Date1, Date2) ), Package).



data_esperada(IdE, Date):-
        package(IdE,_,_,_,_,date(D1,M1,A1,H1),T),
        repara_date(date(D1,M1,A1,(H1+T)),Date).



repara_date(date(DI,MI,AI,HI),_):-
        HI>24, HI=HI-24, DI=DI+1,
        repara_date(date(DI,MI,AI,HI),_).

repara_date(date(DI,MI,AI,HI),_):-
        (
                (member(MI, [2,4,6,9,11]), DI>30, DI=DI-30, MI=MI+1);
                (member(M, [1,3,5,7,8,10,12]), DI>31, DI=DI-31, MI=MI+1)
        ),
        repara_date(date(DI,MI,AI,HI),_).

repara_date(date(DI,MI,AI,HI),_):-
        MI>12, MI=MI-12, AI=AI+1,
        repara_date(date(DI,MI,AI,HI),_).

repara_date(DateI,Date):-
        Date = DateI.



/*
---------------------
Q10 - Calcular o PESO TOTAL transportado por UM estafeta NUM determinado dia

Exemplo: q10(1, date(18,11,2021), P).
---------------------
*/ 

% ---------------------------------------------------
% q10 : EstafetaID, Date, Peso -> {V,F}
% ---------------------------------------------------
% "COMMENT HERE"

q10(EstafetaID, Date, Peso) :-
        
                Date,
                estafeta(EstafetaID,_,_),
                findall(ID, record(ID,_,EstafetaID,Date,_,_), ListaPackage),
                findall(P,(member(ID, ListaPackage), package(ID, P,_,_,_,_,_)), ListaPeso),
                sum_list(ListaPeso, Peso),!.


% ---------------------------------------------------
% q10 : Date, Peso -> {V,F}
% ---------------------------------------------------
% "COMMENT HERE"

q10(Date, Answer) :- 

        findall(E, estafeta(E,_,_), X), sort(X,A),
        q10aux(Date, A, Answer).


% "COMMENT + PASSAR PRA OUTRA FILE"
% ---------------------------------------------------
q10aux(_, [], []).
q10aux(Date, [H|T], [(H,P) | R]) :-
        
        q10(H, Date, P),
        q10aux(Date, T, R).