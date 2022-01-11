:- set_prolog_flag(encoding, utf8) .
:- dynamic transport/4 .
:- dynamic address/2 .
:- dynamic client/2 .
:- dynamic package/7 .
:- dynamic record/6 .
:- dynamic estafeta/3 .
:- discontiguous package/7 .
:- discontiguous record/6 .
:- discontiguous address/2.
:- discontiguous excecao/1.
:- discontiguous transport/5.
:- discontiguous (-)/1.
:- discontiguous nulo/1.
:- discontiguous estafeta/4.
:- discontiguous client/2.



nao(Q) :- Q, !, fail.
nao(_).


si(Q, verdadeiro) :- Q.
si(Q, false) :- -Q.
si(Q, desconhecido) :- nao(Q), nao(-Q).






%%-------------------------------------------------------------------
% transport : Name, Weight, Speed, SpeedLoss, EcoValue               |
%                                                                    |
% Para obter resultados mais eficientes (em escolhas de transportes),|
% é de valor apresentar os que têm menor capacidade primeiro         |
%%-------------------------------------------------------------------

transport('Bicycle', 3, 10, 0.7, 0).
transport('Bicycle', 5, 10, 0.7, 0).

transport('Motorcycle', 12, 35, 0.5, 1).
transport('Motorcycle', 20, 35, 0.5, 1).

transport('Car', 32, 25, 0.1, 2).
transport('Car', 100, 25, 0.1, 2).

transport('Carrinha', 500, 55, 0.07, 3).
transport('Carrinha', 600, 55, 0.07, 3).


-transport(A,B,C,D,E) :-
    nao(transport(A,B,C,D,E)),
    nao((excecao(transport(A,B,C,D,E)))).


-transport('Bicycle', 20, 10, 0.7, 0).
-transport('Avioneta', 150, 55, 0.09, 1).

/*
% conhecimento imperfeito incerto

transport('Bicycle', capacidadeDesconhecida, 10, 0.7, 0).
excecao(transport(A,_,C,D,E)) :- transport(A,capacidadeDesconhecida,C,D,E).


% conhecimento imperfeito impreciso

excecao(transport('Motorcycle', 21), 35, 0.5, 1).
excecao(transport('Motorcycle', 30), 35, 0.5, 1).


% conhecimento imperfeito interdito

transport('Avioneta', capacidadeImpossivel, 55, 0.09, 1).
excecao(transport(A,_,C,D,E)) :- transport(A, capacidadeImpossivel, C,D,E).
nulo(capacidadeImpossivel).

% invariante associado ao conhecimento imperfeito interdito



+transport(A,B,C,D,E) :: (

    solucoes( (A,B,C,D,E), ( transport('Avioneta', capacidadeImpossivel, 55, 0.09, 1), nao(nulo(capacidadeImpossivel)) ), List),
    length(List, 0)
    ).
*/




%%-------------------------------------------------
% GetEcoValue : Transport, EcoValue                |
% Get do valor ecológico de um meio de transporte  |
% usage : getEcoValue( (transport(...)), Answer).  |
%%-------------------------------------------------

getEcoValue(transport(_,_,_,_,E), E).



%%----------------------------------------------------
% GetAllTransports : Transports                       |
% Junta, numa lista, todos os transportes disponíveis |
% usage : getAllTransports(Answer).                   |
%%----------------------------------------------------

getAllTransports(Transports) :-

    findall((transport(A,B,C,D,E)), (transport(A,B,C,D,E)), Transports).



%%-------------------------------------------------------------------------------------------
% Package                                                                                    |
% package : Codigo, Peso, Volume, Valor, Morada, DataCriacao, TempoEspera -> {V,F}           |
% Record                                                                                     |
% record : PackageID, ClientID, EstafetaID, DeliverDate, TransportName, DeliverRate -> {V,F} |
%%-------------------------------------------------------------------------------------------

package(1000000, 2.5, 25 , 30 , address('cantina','uni_norte'), date(11, 11, 2021, 12), 6).
record(1000000, 10000, 1, date(11, 11, 2021, 16), 'Bicycle', 5).

package(1000001, 3.3, 27, 32, address('medicina', 'olimpo'), date(11, 11, 2021, 8), 12).
record(1000001, 10001, 1, date(11, 11, 2021, 20), 'Bicycle', 4). 

package(1000002, 3.4, 20, 33, address('institutoLetras', 'uni_sul'), date(11, 11, 2021, 17), 0).
record(1000002, 10002, 1, date(11, 11, 2021, 18), 'Carrinha', 3). 

package(1000003, 5, 10, 15, address('escolaEngenharia2', 'uni_oeste'), date(12, 11, 2021, 11), 2).
record(1000003, 10002, 1, date(13, 11, 2021, 11), 'Bicycle', 1). 

package(1000004, 1, 5, 10, address('complexoPedagogico1', 'uni_este'), date(13, 11, 2021, 20), 24).
record(1000004, 10003, 3, date(15, 11, 2021, 10), 'Bicycle', 3).

package(1000005, 0.988, 20, 876, address('servicosTecnicos', 'uni_este'), date(10,11,2021, 12), 72). 
record(1000005, 10004, 3, date(11, 11, 2021, 19),'Car', 4).

package(1000006, 13, 102, 2.099, address('bioSustentabilidade', 'uni_oeste'), date(11,11,2021, 9), 12).
record(1000006, 10005, 1, date(11, 11, 2021, 15), 'Bicycle', 5).

package(1000007, 2.112, 122, 44.59, address('escolaEconomia', 'uni_centro'), date(03, 10, 2021, 8), 6).
record(1000007, 10006, 1, date(03, 10, 2021, 20), 'Bicycle', 2).

package(1000008, 88, 122, 443, address('complexoPedagogico3', 'uni_centro'), date(03, 10, 2021, 20), 2).
record(1000008, 10007, 4, date(04, 10, 2021, 8), 'Car', 3).

package(1000009, 88, 122, 200, address('escolaPsicologia', 'uni_oeste'), date(03, 10, 2021, 21), 48).
record(1000009, 10008, 4, date(18, 10, 2021, 12), 'Car', 1).

package(1000010, 28, 2, 67, address('cienciasSociais', 'uni_oeste'), date(23, 12, 2021, 15), 24).
record(1000010, 10020, 2, date(24, 12, 2021, 17), 'Car', 5).

package(1000011, 2300, 150, 120 , address('biblioteca', 'uni_sul'), date(13, 12, 2021, 15), 12).
record(1000011, 10018, 4, date(16, 12, 2021, 17), 'Car', 2).

package(1000012, 70, 0.2 , 0.4 , address('acaoSocial', 'uni_norte'), date(3, 9, 2021, 15), 12).
record(1000012, 10003, 3, date(3, 9, 2021, 17), 'Car', 5).

package(1000013, 5.6, 1, 2, address('medicina','olimpo'), date(2, 7, 2021, 15), 12).
record(1000013, 10016, 1, date(2,7,2021, 17), 'Motorcycle', 4).

package(1000014, 56, 10, 3, address('medicina','olimpo'), date(23, 7, 2021, 15), 48).
record(1000014, 10002, 4, date(24,7,2021, 17), 'Car', 2).

package(1000015, 600, 0.2, 0.1, address('educacao','uni_oeste'), date(30, 9, 2021, 15), 12).
record(1000015, 10003, 2, date(1,10,2021, 17), 'Bicycle', 5).

package(1000016, 45, 2, 3, address('servicosTecnicos','uni_este'), date(10, 3, 2021, 15), 6).
record(1000016, 10012, 2, date(19,3,2021, 17), 'Car', 1).

package(1000017, 5, 6, 3.2, address('escolaCiencias','uni_centro'), date(17, 7, 2021, 15), 0).

% evolucao(record(1000017, 10000, 4, date(18,7,2021,16),'Motorcycle', 1)).




-package(A,B,C,D,E,F,G) :-
    
    nao(package(A,B,C,D,E,F,G)),
    nao(excecao(package(A,B,C,D,E,F,G))).

-package(0, 0, 0, address(escolaEngenharia2, uni_oeste), date(5, 1, 2022, 20), 10).
-package(1, 32, 10, address(escolaCiencias, uni_centro), date(1, 1, 2022, 0), 3).


/*
% conhecimento imperfeito incerto

package(1000020, 100, 32, 99, address(escolaEngenharia1, uni_centro), date(29, 12, 2021, 23), horasEsperaDesconhecidas).
excecao(package(A,B,C,D,E,F,_)) :- package(A,B,C,D,E,F, horasEsperaDesconhecidas).


% conhecimento imperfeito impreciso

excecao(package(1000021, 12, 7, 25, address(medicina, olimpo), date(5,1,2022,21), 3)).
excecao(package(1000021, 12, 7, 25, address(medicina, olimpo), date(5,1,2022,21), 1)).


% conhecimento imperfeito interdito

package(1000022, 30, 30, 76, address(cantina, uni_norte), date(5,1,2021,14), horasEsperaImpossivel).
excecao(package(A,B,C,D,E,F,_)) :- package(A,B,C,D,E,F,horasEsperaImpossivel).
nulo(horasEsperaImpossivel).

% invariante associado ao conhecimento imperfeito interdito

+package(A,B,C,D,E,F,G) :: (

    solucoes((A,B,C,D,E,F,G), ( package(1000022, 30, 30, 76, address(cantina, uni_norte), date(5,1,2021,14), horasEsperaImpossivel), nao(nulo(horasEsperaImpossivel)) ), List),
    length(List, 0)
    ).
*/





-record(A,B,C,D,E,F) :-
    
    nao(record(A,B,C,D,E,F)),
    nao(excecao(record(A,B,C,D,E,F))).


-record(1000016, 10001, 4, address('escolaCiencias', 'uni_centro'), date(17,7.2021,15), 0).
-record(1000018, 2020, 4, address('escolaCiencias', 'uni_centro'), date(13,3.2021,15), 5).


/*
% conhecimento imperfeito incerto

record(1000019, 30000, 6, date(1,12,2021, 23), 'Bicycle', notaDesconhecida).
excecao(record(A,B,C,D,E,_)) :- record(A,B,C,D,E, notaDesconhecida).


% conhecimento imperfeito impreciso

excecao(record(1000021, 30001, 7, date(3,1,2022, 5), 'Bicycle', 1)).
excecao(record(1000021, 30001, 7, date(3,1,2022, 5), 'Bicycle', 3)).


% conhecimento imperfeito interdito

record(1000022, 30002, date(5,1,2022, 12), 'Motorcycle', notaImpossivel).
excecao(record(A,B,C,D,E,_)) :- record(A,B,C,D,E,notaImpossivel).
nulo(notaImpossivel).

% invariante associado ao conhecimento imperfeito interdito

+record(A,B,C,D,E,F) :: (

    solucoes((A,B,C,D,E,F), ( record(1000022, 30002, date(5,1,2022, 12), 'Motorcycle', notaImpossivel), nao(nulo(notaImpossivel)) ), List),
    length(List, 0)
    ).
*/





%%--------------------------------------------------------------
% Estafeta                                                      |
% estafeta : ID, MeioTransporte, [Encomendas], AlgoritmoCaminho |
%%--------------------------------------------------------------

%%- DFS, 8.25 km/h, hour(0,14,32), 2/4 km
estafeta(1, transport('Bicycle', 3, 10, 0.7, 0), [1000000], dfs).


%%- BFS, 7.69 km/h, hour(1, 18, 1), 10/20 km
estafeta(1, transport('Bicycle', 5, 10, 0.7, 0), [1000001], dfs).


%%- DFS, 28.5 km/h, 0.350877 Horas, 10/20 km
estafeta(1, transport('Motorcycle', 20, 35, 0.5, 1), [1000006], dfs).


%%- IDS, 6.5 km/h, 1.3846 horas, 9/18 km 
estafeta(1, transport('Bicycle', 5, 10, 0.7, 0), [1000003], ids).


%%- greedy, 8.5216 km/h, hour(0, 28, 9), 4/8 km
estafeta(1, transport('Bicycle', 3, 10, 0.7, 0), [1000007], greedy).


%%- IDS, 32.2 km/h, 0.37267 horas, 12/24 km
estafeta(1, transport('Motorcycle', 12, 35, 0.5, 1), [1000013], dfs).


%%- aEstrela, 22.2 km/h,  hour(0, 32, 25), 12/24 km
estafeta(2, transport('Car', 32, 25, 0.1, 2), [1000010], astar).


%%- DFS, 54.762 km/h, 0.091304 horas, 5/10 km
estafeta(2, transport('Carrinha', 600, 55, 0.07, 3), [1000002], dfs).


%%- IDS, 12.999 km/h, 0.69230 horas, 9/18 km
estafeta(2, transport('Carrinha', 600, 55, 0.07, 3), [1000015],ids).


%%- DFS, 20.5 km/h, 0.341463 horas, 7/14 km
estafeta(2, transport('Car', 100, 25, 0.1, 2), [1000016], dfs).


%%- BFS, 9.3 km/h, hour(0,38,42), 6/12 km
estafeta(3, transport('Bicycle' , 3, 10, 0.7, 0), [1000004], bfs).


%%- DFS, 9.3084 km/h, hour(0,45,7), 7/14 km
estafeta(3, transport('Bicycle', 3, 10, 0.7, 0), [1000005], dfs).


%%- IDS, 18 km/h, hour(0,16,40), 5/10 km
estafeta(3, transport('Car', 100, 25, 0.1, 2),[1000012], ids).


%%- DFS, 16.2 km/h, hour(0,7,24), 2/4 km
estafeta(4, transport('Car', 100, 25, 0.1, 2), [1000008], dfs).


%%- BFS, 1 km/h, 6 horas, 6/12 km
estafeta(4, transport('Car', 100, 25, 0.1, 2), [1000011], bfs).


%%- DFS, 19.4 km/h, hour(0, 27, 50), 9/18 km
estafeta(4, transport('Car', 100, 25, 0.1, 2), [1000014], dfs).


%%- BFS, 16.2 km/h, hour(0, 37, 2), 10/20 km
estafeta(4, transport('Car', 100, 25, 0.1, 2),[1000009], bfs).


%%- IDS, 54.65, 0.05 horas, 3/6 km
estafeta(4, transport('Carrinha', 600, 55, 0.07, 3),[1000017], ids).



% conhecimento perfeito negativo

-estafeta(A,B,C,D) :-
    
    nao(estafeta(A,B,C,D)),
    nao(excecao(estafeta(A,B,C,D))).


-estafeta(4, transport('Bicycle', 3, 10, 0.7, 0), [1000017], dfs).
-estafeta(0, transport('Car', 100, 25, 0.1, 2), [0101], dfs).

/*
% conhecimento imperfeito incerto

estafeta(estafetaDesconhecido, transport('Motorcycle', 12, 35, 0.5, 1), [1000020], dfs).
excecao(estafeta(_,B,C,D)) :- transport(estafetaDesconhecido, B,C,D).


% conhecimento imperfeito impreciso

excecao(estafeta(5, transport('Motorcycle', 12, 35, 0.5, 1), [10000021])).
excecao(estafeta(6, transport('Motorcycle', 12, 35, 0.5, 1), [10000021])).


% conhecimento imperfeito interdito

estafeta(estafetaImpossivel, transport('Bicycle', 5, 10, 0.7, 0), [10000022]).
excecao(estafeta(_,B,C,D)) :- transport(estafetaImpossivel, B,C,D).
nulo(estafetaImpossivel).

% invariante associado ao conhecimento imperfeito interdito

+estafeta(A,B,C,D) :: (

    solucoes((A,B,C,D), ( estafeta(estafetaImpossivel, transport('Bicycle', 5, 10, 0.7, 0), [10000022]), nao(nulo(estafetaImpossivel)) ), List),
    length(List, 0)
    ).

*/



%--------------------- AUXILIAR
%- Verificar se todos os package ID, presentes numa lista, existem na base de dados

validate_pkg_unicity([]).
validate_pkg_unicity([H | T]) :- 
                
                package(H,_,_,_,_,_,_),
                validate_pkg_unicity(T).

%--------------------- AUXILIAR
%- Verificar se todos os package ID, presentes numa lista, estão por entregar, i.e., não há um record sobre eles

validate_to_deliver([]).
validate_to_deliver([H | T]) :- 
                
                \+record(H,_,_,_,_,_),
                validate_to_deliver(T).
         





%%---------------------------
% Client                     |
% client : ID, Nome -> {V,F} |
%%---------------------------

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


-client(A,B) :-

    nao(client(A,B)),
    nao(excecao(cliente(A,B))).


-client(0, 'Admin').
-client(1, 'Co-Worker').

/*
% conhecimento imperfeito incerto

client(2, nomeDesconhecido).
excecao(client(A,_)) :- client(A,nomeDesconhecido).


% conhecimento imperfeito impreciso

excecao(client(3, 'Paulo Araújo')).
excecao(client(4, 'Paulo Silva')).


% conhecimento imperfeito interdito

client(5, nomeImpossivel).
excecao(client(A,_)) :- client(A, nomeImpossivel).
nulo(nomeImpossivel).

% invariante associado ao conhecimento imperfeito interdito

+client(A,B) :: (

    solucoes((A,B), ( client(5,nomeImpossivel), nao(nulo(nomeImpossivel)) ), List),
    length(List, 0)
    ).

*/

%%-----------------------------------
% Address                            |
% address : Rua, Freguesia -> {V,F}  |
%%-----------------------------------

address( complexoPedagogico1, uni_este).
address( complexoPedagogico2, uni_sul).
address( complexoPedagogico3, uni_centro).
address( biblioteca, uni_sul).
address( institutoLetras, uni_sul).
address( escolaCiencias, uni_centro).
address( escolaEngenharia1, uni_centro).
address( escolaEconomia, uni_centro).
address( complexoDesportivo, uni_este).
address( servicosTecnicos, uni_este).
address( cantina, uni_norte).
address( acaoSocial, uni_norte).
address( educacao, uni_oeste).
address( psicologia, uni_oeste).
address( cienciasSociais, uni_oeste).
address( escolaEngenharia2, uni_oeste).
address( escolaDireito, uni_oeste).
address( bioSustentabilidade, uni_oeste) .
address( medicina, olimpo).


-address(A,B) :-(

    nao(address(A,B)),
    nao(excecao(address(A,B)))).


-address(ruaDeSouto, saoJoaoDoSouto).
-address(avenidaLiberdade, saoJoseSaoLazaro).


/*
% conhecimento imperfeito incerto

address(populo, freguesia_desconhecida).
excecao(address(A,_)) :- address(A,freguesia_desconhecida).


% conhecimento imperfeito impreciso

excecao(address(ruaDaUniversidade, gualtar)).
excecao(address(ruaDaUniversidade, lamacaes)).


% conhecimento imperfeito interdito

address(ruaConegoDoutorFaria, freguesia_impossivel).
excecao(address(A,_)) :- address(A, freguesia_impossivel).
nulo(freguesia_impossivel).

% invariante associado ao conhecimento imperfeito interdito

+address(A,B) :: (

    solucoes((A,B), ( address(ruaConegoDoutorFaria,freguesia_impossivel), nao(nulo(freguesia_impossivel)) ), List),
    length(List, 0)
    ).

*/

%%----------------------------------------
% Address Auxiliar                        |
% ValidateAddress : Address               |
% Verifica se uma rua é valida e existe   |
%%----------------------------------------

validate_address(address(R, F)) :-
    atom(R), atom(F), address(R,F).
