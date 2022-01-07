%----------------------
% File written by------
% ---------------G28---
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload

% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').
:- consult('Auxiliares.pl').
:- consult('Invariants.pl').
:- consult('texts.pl').
:- consult('Util.pl').

:- discontiguous q5/3 .
:- discontiguous q4/2 .

:- write("#> \"queries()\" to know what queries are available ..."),nl,nl.


newClient(ID, NAME) :- evolucao(client(ID,NAME)).
newAddress(RUA, FREGUESIA) :- evolucao(address(RUA, FREGUESIA)).
newEstafeta(ID, TRANSPORT, LIST) :- evolucao(estafeta(ID, TRANSPORT, LIST)).
newRecord(PKGID, CID, EID, DDATE, TRANSPORTNAME, RATE) :- evolucao(record(PKGID, CID, EID, DDATE, TRANSPORTNAME, RATE)).
newPackage(ID, W, V, P, ADD, DATE, HOURS) :- evolucao(package(ID, W, V, P, ADD, DATE, HOURS)).
newTransport(NAME, MAXWEIGHT, AVGSPEED, ECOVALUE) :- evolucao(transport(NAME, MAXWEIGHT, AVGSPEED, ECOVALUE)).


remClient(ID, NAME) :- involucao(client(ID,NAME)).
remAddress(RUA, FREGUESIA) :- involucao(address(RUA, FREGUESIA)).
remEstafeta(ID, TRANSPORT, LIST) :- involucao(estafeta(ID, TRANSPORT, LIST)).
remRecord(PKGID, CID, EID, DDATE, TRANSPORTNAME, RATE) :- involucao(record(PKGID, CID, EID, DDATE, TRANSPORTNAME, RATE)).
remPackage(ID, W, V, P, ADD, DATE, HOURS) :- involucao(package(ID, W, V, P, ADD, DATE, HOURS)).
remTransport(NAME, MAXWEIGHT, AVGSPEED, ECOVALUE) :- involucao(transport(NAME, MAXWEIGHT, AVGSPEED, ECOVALUE)).

/*
---------------------
Query1,  Estafeta que usou (+) vezes um meio de transporte (+) ecológico.
---------------------
*/


% V1 -> q1 : EstafetaID, Transport(_,_,_,_), Count -> {V,F}
% ---------------------------------------------------------

q1(ID, Meio, Answer) :- 
        
        findall((ID,N), (estafeta(ID,Meio,Pkgs,_), length(Pkgs,N)),X),
        max_couple(X, Answer), writeq1(Answer).



% V2 -> q1 : EstafetaID, 'Name', Count -> {V,F}
% ---------------------------------------------------------

q1(ID, M, A) :-
        
        transport(M,_,_,_),!,
        findall((ID,N), (estafeta(ID,transport(M,_,_,_),Pkgs,_), length(Pkgs,N)), X),
        max_couple(X, A), writeq1(A).



% writeq1 : Escreve a informação relativa a query1.
% ---------------------------------------------------------

writeq1((ID,N)) :-

        nl,
        write("Estafeta ["), write(ID), write("] usou "), write(N), write(" vezes"),nl.




/*
---------------------
Query2,  Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente.
---------------------
*/  


% V1 -> q2 : Client, [(Encomenda, Estafeta)] -> {V,F}
% Dado um cliente, devolve a lista de (Encomenda, Estafeta) que lhe estão associados.
% ---------------------------------------------------------

q2(Client, List) :-

        verify_client(Client),
        clientID(Client, Aux),
        findall((Enc,Est), record(Enc, Aux, Est,_,_,_), List),
        writeq2(List),!.



% V2 -> q2 : Client, Estafeta, [Encomendas] -> {V,F} |
% Dado um cliente e um estafeta, devolve uma lista com todas as encomendas que foram entregues àquele cliente, por aquele estafeta.
% ---------------------------------------------------------

q2(Client, Estafeta, Encomendas) :-

        verify_client(Client),
        estafeta(Estafeta, _, _),
        clientID(Client, Aux),
        findall(Enc, record(Enc, Aux, Estafeta,_,_,_), Encomendas),nl,
        writeq2_1(Encomendas), !.



% V3 -> q2 : Client, [Encomendas], [(Encomenda, Estafeta)] -> {V,F} |
% Dado um cliente e uma lista de encomendas, devolve uma lista de encomendas associadas ao respetivo estafeta da entrega.
% ---------------------------------------------------------

q2(Client, Encomendas, List) :- 

        verify_client(Client),
        clientID(Client, Aux),
        findall((Enc,Est), (record(Enc, Aux, Est,_,_,_), member(Enc, Encomendas)), List),nl,
        writeq2(List).



% writeq2 : Escreve a informação relativa a query2.
% ---------------------------------------------------------

writeq2([]).
writeq2([(H,E) | T]) :- 

        write("Encomenda "), write(H), write(", Estafeta ["), write(E), write("]"),nl,
        writeq2(T).

writeq2_1([]).
writeq2_1([H | T]) :-

        write("Encomenda "), write(H),nl,
        writeq2_1(T).




/*
---------------------
Query3, Identificar os clientes servidos por um determinado estafeta.
---------------------
*/


% V1 -> q3 : Estafeta, [Clients] -> {V,F} 
% Dado um estafeta, devolve que clientes receberam encomendas desse mesmo estafeta.
% ---------------------------------------------------------

q3(Estafeta, A) :-
                            
        findall(X, record(_, X, Estafeta,_,_,_), L),
        sort(L, A),
        write_q3(Estafeta, A), !.



% V2 -> q3 : Client, [Estafetas] -> {V,F} 
% Dado um cliente, devolve a lista de estafetas que lhe fizeram entregas.
% ---------------------------------------------------------

q3(Client, Answer) :-
            
        verify_client(Client),
        clientID(Client, Aux),
        findall(Est, record(_, Aux, Est,_,_,_), Estafetas),
        sort(Estafetas, Answer),
        write_q3_1(Client, Answer),!.



% writeq3 : Escreve a informação relativa a query3.
% ---------------------------------------------------------

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

write_q3_1(Client, [H]):-

        write("Estafeta: "), write(H), write(" "),nl,
        write("Client: "), write(Client),nl,nl,!.

write_q3_1(C, [H|T]) :-

        write("Estafeta: "), write(H), write(" "),nl,
        write_q3_1(C,T).




/*
---------------------
Query4, Calcular o valor faturado num determinado dia.
---------------------
*/


% V1 -> q4 : Date, Value -> {V,F}
% Dada uma data, devolve o total de dinheiro que a empresa fez nesse dia.
% ---------------------------------------------------------

q4(date(D,M,Y,_), Value) :-
                
        date(D,M,Y,0),!,
        findall(ID, record(ID,_,_,date(D,M,Y,_),_,_), IDS),
        custo(IDS, Value), writeq4(Value).



% V2 -> q4 : Month, Year, Value -> {V,F}    
% Dados um mês e ano, devolve o valor ganho nessa data.
% ---------------------------------------------------------

q4(M, Y, Value) :- 
                
        member(M, [1,2,3,4,5,6,7,8,9,10,11,12]),
        Y > 0,
        findall(ID, record(ID,_,_,date(_,M,Y,_),_,_), IDS),
        custo(IDS, Value), writeq4(Value).



% V3 -> q4 : Year, Value -> {V,F}           
% Dado um ano, calcula o valor ganho nesse ano.
% ---------------------------------------------------------

q4(Y, Value) :- 

        Y > 0,
        findall(ID, record(ID,_,_,date(_,_,Y,_),_,_), IDS),
        custo(IDS, Value), writeq4(Value).



% custo : função auxiliar que calcula o custo de um conjunto de encomendas.
% ---------------------------------------------------------

custo([ID], V) :-
        
        package(ID,A,B,C,_,_,H),
        total_price(package(ID,A,B,C,_,_,H), V).


custo([H|T], V) :-
        
        package(H,A,B,C,_,_,Hr), 
        total_price(package(H,A,B,C,_,_,Hr), V1),
        custo(T,V2),
        V is V1+V2.



% writeq4 : Escreve a informação relativa a query4.
% ---------------------------------------------------------

writeq4(Value) :-

        nl,
        write("Value gained: "),write(Value),nl,nl.




/*
---------------------
Query5, Identificar as zonas (rua, freguesia) com maior volume de entregas por parte do Green Distribution.
---------------------
*/


% V1 -> q5 : Address, NumberOfDeliveries, ListOfPackages -> {V,F}
% Recebendo uma freguesia, devolve o nº de entregas realizadas nessa localização.
% ---------------------------------------------------------

q5(Freguesia, Volume, Delivered) :- 

        address(_,Freguesia),!,
        findall(PID, package(PID, _,_,_, address(_, Freguesia), _,_), List),
        findall(Id, (member(Id, List), record(Id,_,_,_,_,_)), Delivered),
        length(Delivered, Volume),
        writeq5_01(Freguesia, Volume, Delivered).



% V2 -> q5 : Address, NumberOfDeliveries, ListOfPackages -> {V,F}
% Recebendo um address completo, devolve o nº de entregas realizadas nessa localização.
% ---------------------------------------------------------

q5(address(R,F), Volume, Delivered) :-
        
        atom(R), atom(F),
        address(R,F),
        findall(PID, package(PID, _,_,_, address(R, F), _,_), List),
        findall(Id, (member(Id, List), record(Id,_,_,_,_,_)), Delivered),
        length(Delivered, Volume),
        writeq5_02(R,F,Volume,Delivered).



% writeq5 : Escreve a informação relativa a query5.
% ---------------------------------------------------------

writeq5_01(F,V,D) :- 
        
        nl,
        write("Freguesia: "), write(F),nl,
        write("Numero entregas: "), write(V),nl,
        write("Encomendas: "), write(D),nl,nl,!.

writeq5_02(R,F,V,D) :-
        
        nl,
        write("Rua: "), write(R),nl,
        write("Freguesia: "), write(F),nl,
        write("Numero entregas: "), write(V),nl,
        write("Encomendas: "), write(D),nl,nl,!.




/*
---------------------
Query6 - Calcular a classificação média de satisfação de cliente para um determinado estafeta.
---------------------
*/


% V1 -> q6 : Estafeta, Value -> {V,F}
% Dado um estafeta, calcula a média geral que os clientes lhe dão.
% ---------------------------------------------------------

q6(Estafeta, Value) :-
            
        estafeta(Estafeta, _, _),
        findall(X, record(_,_,Estafeta,_, _,X), L),!,
        average(L,Value),
        writeq6(Estafeta, Value),!.



% V2 -> q6 : Client, Estafeta, Value -> {V,F}
% Dado um cliente e um estafeta, calcula a média que esse cliente atribuiu ao estafeta.
% ---------------------------------------------------------

q6(Client, Estafeta, Value) :-
                
        verify_client(Client),
        clientID(Client, ID),
        findall(X, record(_, ID, Estafeta, _, _,X), L),
        average(L, Value),
        writeq6(Estafeta, Value),!.



% writeq6 : Escreve a informação relativa a query6.
% ---------------------------------------------------------

writeq6(Estafeta,Value) :-
    write("Estafeta ["),write(Estafeta),write("] Nota: "),format("~2f",Value), nl,nl.




/*
---------------------
Query7, Calcular o nº de entregas pelos diferentes meios de transporte, num intervalo de tempo.
---------------------
*/


% V1 -> q7 : Data, Answer -> {V,F}
% Dada uma data, calcula o numero de entregas feitas pelos diferentes meios de transporte nesse dia.
% ---------------------------------------------------------

q7(date(D,M,Y,_), Answer) :-
        
        date(D,M,Y,0),
        findall(ID, record(ID,_,_,date(D,M,Y,_),_,_), ListaPackage), 
        findall(X, (member(P, ListaPackage), record(P,_,_,_,X,_)), List),
        agrupa(List, Answer),!,
        writeq7(Answer).



% V2 -> q7 : Data, Data, Answer -> {V,F}
% Dadas 2 datas, calcula o numero de entregas feitas pelos diferentes meios de transporte num intervalo de tempo.
% ---------------------------------------------------------

q7(Date1,Date2,Answer) :-

        (Date1, Date2),
        isAfter(Date1,Date2),
        filter_by_date_Transport(Date1, Date2, Transport),
        agrupa(Transport, Answer),!, writeq7(Answer).



% writeq7 : Escreve a informação relativa a query7.
% ---------------------------------------------------------

writeq7([]) :- nl.
writeq7([(H,NTimes) | Resto]) :-

        write("["),
        write(NTimes), write("] package(s) by "), write(H), nl,
        writeq7(Resto).




/*
---------------------
Query8, Calcular o n de entregas por estafeta, num intervalo de tempo.
---------------------
*/ 


% V1 -> q7 : Data, Answer -> {V,F}
% Dada uma data, calcula o numero de entregas feitas, por cada estafeta, nesse dia.
% ---------------------------------------------------------

q8(date(D,M,Y,_),Answer) :-

        date(D,M,Y,0),
        findall(ID, record(_,_,ID,date(D,M,Y,_),_,_), ListaEst),
        agrupa(ListaEst,Answer),!,
        writeq8(Answer).



% V1 -> q7 : Data, Answer -> {V,F}
% Dadas 2 datas, calcula o numero de entregas feitas, por cada estafeta, num intervalo de tempo.
% ---------------------------------------------------------

q8(Date1,Date2, Answer) :-

        (Date1, Date2),
        isAfter(Date1,Date2),
        filter_by_date_estafetas(Date1, Date2, EstafetasList),
        agrupa(EstafetasList,Answer),!,
        writeq8(Answer).



% writeq8 : Escreve a informação relativa a query8.
% ---------------------------------------------------------

writeq8([]) :- nl.
writeq8([(H,NTimes) | Resto]) :-

        write("Courier ["), write(H), write("] delivered ["), write(NTimes), write("] packages."), nl,
        writeq8(Resto).




/*
---------------------
Query9, calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo.
---------------------
*/ 


% query9 : Data, Data, Total, Entregues, NEntregues -> {V,F}
% Dadas 2 datas, calcula o numero de encomendas entregues, nao entregues e total, num determinado periodo de tempo.
% ---------------------------------------------------------

q9(Date1, Date2, Total, Entregues, NEntregues):- 

        (Date1, Date2),
        isAfter(Date1,Date2),
        filter_by_date_encomendas(Date1, Date2, Encomendas),
        length(Encomendas,Total),
        findall(E, 
                        (
                        member(E, Encomendas),
                        record(E,_,_,DE,_,_),
                        data_esperada(E,DP),
                        isAfter(DP,DE)
                        )
                ,
                EncomendasConcluidas),
        length(EncomendasConcluidas, Entregues),
        NEntregues is Total - Entregues,
        writeq9(Total, Entregues, NEntregues).



% filter_by_date_encomendas : função auxiliar que filtra encomendas da base de dados de acordo com a sua data_esperada.
% ---------------------------------------------------------

filter_by_date_encomendas(Date1, Date2, Package) :-

        findall(E, ( package(E,_,_,_,_,_,_), data_esperada(E,DATE), dateInBetween(DATE, Date1, Date2) ), Package).



% data_esperada : função auxiliar que calcula a data maxima em que a encomenda deve ser entregue.
% ---------------------------------------------------------

data_esperada(IdE, Date):-

        package(IdE,_,_,_,_,date(D1,M1,A1,H1),T),
        once(repara_date(date(D1,M1,A1,(H1+T)),Date)).



% repara_date : função auxiliar que torna uma data com elementos impossíveis como *horas = 25* numa data real.
% ---------------------------------------------------------

repara_date(date(DI,MI,AI,HI),Date):-

        HI>24, HF is HI-24, DF is DI+1,
        repara_date(date(DF,MI,AI,HF),Date).

repara_date(date(DI,MI,AI,HI),Date):-

        (
                (member(MI, [2,4,6,9,11]), DI>30, DF is DI-30, MF is MI+1);
                (member(MI, [1,3,5,7,8,10,12]), DI>31, DF is DI-31, MF is MI+1)
        ),
        repara_date(date(DF,MF,AI,HI),Date).

repara_date(date(DI,MI,AI,HI),Date):-

        MI>12, MF is MI-12, AF is AI+1,
        repara_date(date(DI,MF,AF,HI),Date).

repara_date(DateI,Date):-

        Date = DateI.



% writeq9 : Escreve a informação relativa a query9.
% ---------------------------------------------------------

writeq9(T, E, N):-
        write("Total:"), write(T),nl,
        write("Total entregues:"), write(E),nl,
        write("Total não entregues:"), write(N),nl,nl.

 


/*
---------------------
Q10 - Calcular o PESO TOTAL transportado por UM estafeta NUM determinado dia.
---------------------
*/ 


% V1 -> q10 : EstafetaID, Date, Peso -> {V,F}
% Dado um estafeta e uma data, calcula o peso transportado pelo mesmo nessa data.
% ---------------------------------------------------------

q10(EstafetaID, date(D,M,Y,_), Peso) :-
        
        date(D,M,Y,0),
        estafeta(EstafetaID,_,_,_),
        findall(ID, record(ID,_,EstafetaID, date(D,M,Y,_),_,_), ListaPackage),
        findall(P,(member(ID, ListaPackage), package(ID, P,_,_,_,_,_)), ListaPeso),
        sum_list(ListaPeso, Peso),!.



% V2 -> q10 : Date, Peso -> {V,F}
% Dada uma data, calcula o pedo transportado por cada estafeta nessa data.
% ---------------------------------------------------------

q10(Date, Answer) :- 

        findall(E, estafeta(E,_,_,_), X), sort(X,A),
        q10aux(Date, A, Answer),
                writeq10(Answer).



% q10aux : função auxiliar que recrursivamente, calcula a resposta para a V2 da query10.
% ---------------------------------------------------------

q10aux(_, [], []).
q10aux(Date, [H|T], [(H,P) | R]) :-
        
        q10(H, Date, P),
        q10aux(Date, T, R).



% writeq10 : Escreve a informação relativa a query10.
% ---------------------------------------------------------

writeq10([]) :- nl.
writeq10([(H,Peso) | Resto]) :-
    write("Courier ["), write(H), write("] -> ["), write(Peso), write("] kg."), nl,
     writeq10(Resto).