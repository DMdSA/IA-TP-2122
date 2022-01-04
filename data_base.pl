:- set_prolog_flag(encoding, utf8).

:- dynamic transport/4 .
:- dynamic address/2 .
:- dynamic client/2 .
:- dynamic package/7 .
:- dynamic record/6 .
:- dynamic estafeta/3 .

:- discontiguous package/7 .
:- discontiguous record/6 .



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

    findall((transport(A,B,C,D,E)), transport(A,B,C,D,E), Transports).


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

package(1000002, 3.4, 20, 33, address('intitutoLetras', 'uni_sul'), date(11, 11, 2021, 17), 0).
record(1000002, 10002, 1, date(11, 11, 2021, 18), 'Bicycle', 3). 

package(1000003, 5, 10, 15, address('escolaEngenharia2', 'uni_oeste'), date(12, 11, 2021, 11), 2).
record(1000003, 10002, 1, date(13, 11, 2021, 11), 'Motorcycle', 1). 

package(1000004, 1, 5, 10, address('complexoPedagogico1', 'uni_este'), date(13, 11, 2021, 20), 24).
record(1000004, 10003, 3, date(15, 11, 2021, 10), 'Car', 3).

package(1000005, 0.988, 20, 876, address('servicosTecnicos', 'uni_este'), date(17,11,2021, 12), 72). 
record(1000005, 10004, 3, date(18, 11, 2021, 19),'Car', 4).

package(1000006, 13, 102, 2.099, address('bioSustentabilidade', 'uni_oeste'), date(11,11,2021, 9), 12).
record(1000006, 10005, 1, date(11, 11, 2021, 15), 'Bicycle', 5).

package(1000007, 2.112, 122, 44.59, address('escolaEconomia', 'uni_centro'), date(03, 10, 2021, 8), 6).
record(1000007, 10006, 1, date(03, 10, 2021, 20), 'Motorcycle', 2).

package(1000008, 88, 122, 443, address('complexoPedagogico3', 'uni_centro'), date(03, 10, 2021, 20), 2).
record(1000008, 10007, 4, date(04, 10, 2021, 8), 'Car', 3).

package(1000009, 88, 122, 200, address('psicologia', 'uni_oeste'), date(03, 10, 2021, 21), 48).
record(1000009, 10008, 4, date(18, 10, 2021, 12), 'Car', 1).

package(10000010, 28, 2, 67, address('cienciasSociais', 'uni_oeste'), date(23, 12, 2021, 15), 24).
record(10000010, 10020, 2, date(24, 12, 2021, 17), 'Bicycle', 5).

package(1000011, 2300, 150, 120 , address('biblioteca', 'uni_sul'), date(13, 12, 2021, 15), 12).
record(1000011, 10018, 4, date(16, 12, 2021, 17), 'Car', 2).

package(1000012, 70, 0.2 , 0.4 , address('acaoSocial', 'uni_norte'), date(3, 9, 2021, 15), 12).
record(1000012, 10003, 3, date(3, 9, 2021, 17), 'Bicycle', 5).

package(1000013, 5.6, 1, 2, address('medicina','olimpo'), date(2, 7, 2021, 15), 12).
record(1000013, 10016, 1, date(2,7,2021, 17), 'Motorcycle', 4).

package(1000014, 56, 10, 3, address('complexoDesportivo','uni_este'), date(23, 7, 2021, 15), 48).
record(1000014, 10002, 4, date(24,7,2021, 17), 'Car', 2).

package(1000015, 600, 0.2, 0.1, address('educacao','uni_oeste'), date(30, 9, 2021, 15), 12).
record(1000015, 10003, 2, date(1,10,2021, 17), 'Bicycle', 5).

package(1000016, 45, 2, 3, address('servicosTecnicos','uni_este'), date(10, 3, 2021, 15), 6).
record(1000016, 10012, 2, date(19,3,2021, 17), 'Car', 1).

package(1000017, 5, 6, 3.2, address('escolaCienciass','uni_centro'), date(17, 7, 2021, 15), 0).

% evolucao(record(1000017, 10000, 4, date(18,7,2021,16),'Motorcycle', 1)).







%%--------------------------------------------------------------
% Estafeta                                                      |
% estafeta : ID, MeioTransporte, [Encomendas], AlgoritmoCaminho |
%%--------------------------------------------------------------


%%- DFS, 8.25 km/h, 2.424242 Horas, 20 km
estafeta(1, transport('Bicycle', 5, 10, 0.7, 0), [1000000], dfs).


%%- BFS, 7.69 km/h, 10.403120936 Horas, 80 km
estafeta(1, transport('Bicycle', 5, 10, 0.7, 0), [1000001], bfs).

%%- DFS, 28.5 km/h, 1.75438 Horas, 50 km
estafeta(1, transport('Bicycle', 5, 10, 0.7, 0), [1000006], dfs).

%%- NÃO CUMPRE REQUISITOS DE TEMPO
% estafeta(1, transport('Motorcycle', 12, 35, 0.5, 1), [1000003]).

%%- IDS, 8.5216 km/h, 1.64288 horas, 14 km
estafeta(1, transport('Bicycle',5,10,0.7, 0), [1000007], ids).

%%- IDS, 32.2 km/h, 2.7950310 horas, 90 km
estafeta(1, transport('Motorcycle', 20, 35, 0.5, 1), [1000013], ids).

%%- BFS, 22.2 km/h, 2.702702 horas, 60 km
estafeta(2, transport('Car', 100, 25, 0.1, 2), [10000010], bfs).

%%- NÃO CUMPRE REQUISITOS DE TEMPO
% estafeta(2, transport('Bicycle', 3, 10, 0.7, 0), [1000002]).

%%- NÃO CUMPRE REQUISITOS (2000 KG) 
% estafeta(2, transport('Bicycle', 3, 10, 0.7, 0), [1000015]).

%%- DFS, 20.5 km/h, 2.4390 horas, 50 km
estafeta(2, transport('Car', 100, 25, 0.1, 2), [1000016], dfs).

%%- BFS, 9.3 km/h, 2.36559 horas, 22 km
estafeta(3, transport('Bicycle' , 5, 10, 0.7, 0), [1000004], bfs).

%%- DFS, 9.3084 km/h, 5.371492 horas, 50 km
estafeta(3, transport('Bicycle', 5, 10, 0.7, 0), [1000005], dfs).

%%- IDS, 18 km/h, 1.3333 horas, 24 km
estafeta(3, transport('Car', 100, 25, 0.1, 2),[1000012], ids).

%%- NÃO CUMPRE REQUISITOS TEMPO
% estafeta(4, transport('Car', 100, 25, 0.1, 2), [1000008]).

%%- NÃO CUMPRE REQUISITOS, > 2000 KG
% estafeta(4, transport('Car', 100, 25, 0.1, 2), [1000011]).

%%- DFS, 19.4 km/h, 1.6494 horas, 32 km
estafeta(4, transport('Car', 100, 25, 0.1, 2), [1000014], dfs).

%%- BFS, 16.2 km/h, 4.320987 horas, 70 km
estafeta(4, transport('Car', 100, 25, 0.1, 2),[1000009], bfs).

%%- NÃO CUMPRE REQUISITOS TEMPO
% estafeta(4, transport('Motorcycle', 20, 35, 0.5, 1),[1000017]).






/*

                                                        \\ "GUIDE" //

Como testar os estafetas - caminhos - etc ?


+ melhor caminho : melhorSolucaoDFS( (rua, freguesia), Caminho, Custo).
            ""      ""  BFS     ""          ""
            ""      ""  IDS     ""          ""


+ caminhos possiveis : circuitoDFS( (rua, freguesia), Caminho, Custo).


+ melhor caminho com escolha + ecologica : indicadorProdutividadeDFS(packageid, (rua, freguesia), EcoTransport, Caminho, Km).


+ Lista de transportes associados À velocidade média e ao tempo, em horas, :
  Dado um peso de um package e a distancia do caminho que vai percorrer, devolve essa lista

    getAllTransportsInfo(pesoDoPackage, KM, List).


+ Lista com todos os transportes associados à media da velocidade e ao tempo que durou a viagem, mas que respeitam os limites
    de peso, velocidade, etc (indicadores de produtividade)

    getAllPossibleTransports(packageid, KM, List). 

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



%%-----------------------------------
% Address                            |
% address : Rua, Freguesia -> {V,F}  |
%%-----------------------------------

address( complexoPedagogico1, uni_este).
address( complexoPedagogico2, uni_sul).
address( complexoPedagogico3, uni_centro).
address( biblioteca, uni_sul).
address( intitutoLetras, uni_sul).
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


%%----------------------------------------
% Address Auxiliar                        |
% ValidateAddress : Address               |
% Verifica se uma rua é valida e existe   |
%%----------------------------------------

validate_address(address(R, F)) :-
    atom(R), atom(F), address(R,F).