%----------------------
% File written by------
% ---------------G28---
%----------------------

% make. to reload

% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- discontiguous indicadorProdutividadeGreedy/5.
 



%%------------------------------------------------------------------
% "Circuito com DFS"
% CircuitoDFS : (Rua, Freguesia), Caminho, CustoIda, Custo)         |
% usage : circuitoDFS((rua, freguesia), Cam, CustoIda, CustoFinal). |
%%------------------------------------------------------------------

circuitoDFS(PontoEntrega, CaminhoFinal, CustoIDA, CustoFinal) :-

  ida(PontoEntrega, Caminho1Aux, CustoIDA),

  volta(PontoEntrega, Caminho2Aux, Custo2),

  tail(Caminho2Aux, Caminho2),

  append(Caminho1Aux, Caminho2, CaminhoFinal),

  CustoFinal is CustoIDA + Custo2.



%%-------------------------------------------------------
% "DFS com lista de packages"                            |
% circuitoDFSL : List of PackageID, Caminho, Distancia   |
% usage : circuitoDFSL([packageID], Caminho, Distancia). |
%%-------------------------------------------------------

circuitoDFSL([PackageID | Resto],Caminho,Kms) :-
  list_addresses([PackageID | Resto],List),
  dfsl((escolaEngenharia1,uni_centro),List,Caminho,Kms).



%%-------------------------------------------------------------------------------------------------------------------
% "Depth first search, Indicador de produtividade"                                                                   |
% indicadorProdutividadeDFS : PackageID, EcoTransportation, Caminho, Ida, Km                                         |
% usage : indicadorProdutividadeDFS(packageid, (rua, freguesia), EcoT, Cam, CustoIda, CustoTotal).                   |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeDFS(PackageID, EcoTransportation, Caminho, Ida, Km) :-
    
    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    melhorSolucaoDFS((Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).







%%------------------------------------------------------------------
% "Circuito Breadth first search"
% Circuito : Destino, Caminho, CustoIda, CustoFinal                 |
% usage : circuitoBFS((rua, freguesia), Cam, CustoIda, CustoFinal). |
%%------------------------------------------------------------------

circuitoBFS(Dest,Caminho, CustoIDA, Custo):-
  
  %%- ida
  bfs2(Dest,[[(escolaEngenharia1, uni_centro)]], Cam1),

  %%- volta
  bfs2((escolaEngenharia1, uni_centro), [[Dest]], Cam2),

  distance(Cam1, CustoIDA),

  distance(Cam2, Custo2),

  tail(Cam2, Cam2Aux),

  append(Cam1, Cam2Aux, Caminho),

  Custo is CustoIDA + Custo2.




%%-------------------------------------------------------------------------------------------------------------------
% "Breadth first search, Indicador de produtividade"                                                                 |
% indicadorProdutividadeBFS : PackageID, EcoTransportation, Caminho, Ida, Km                                         |
% usage : indicadorProdutividadeBFS(packageid, EcoT, Cam, CustoIda, CustoTotal).                                     |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeBFS(PackageID, EcoTransportation, Caminho, Ida, Km) :-
    
    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),
    
    melhorSolucaoBFS((Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).



%%-----------------------------------------------------------
% "Circuito Breadth first search, lista de packages"         |
% circuitoBFSL : packageIDs list, Caminho, Kms               |
% usage : circuitoBFSL([packagesID],Caminho,Distancia).      |
%%-----------------------------------------------------------

circuitoBFSL([PackageID | Resto],Caminho,Custo) :-
  
  list_addresses([PackageID | Resto],List),
  
  bfsl_aux((escolaEngenharia1,uni_centro),List,Caminho,Custo).


%%--------------------------------------------------------------------
% "Circuito Iterative Deepening Search"
% CircuitoIDS : PontoEntrega, Caminho, CustoIda, Custo                |
% realiza um circuito completo desde um ponto inicial pré-definido    |
% (no corpo da regra), até um ponto de entrega pedido                 |
% usage : circuitoIDS((rua, freguesia), Cam, CustoIda, CustoFinal).   |
%%--------------------------------------------------------------------

circuitoIDS(PontoEntrega, CaminhoFinal, CustoIda, CustoFinal) :-

  path((escolaEngenharia1, uni_centro), PontoEntrega, CustoIda, Caminho1),
  reverse(Caminho1, Caminho1Final),

  path(PontoEntrega, (escolaEngenharia1, uni_centro), Custo2, Caminho2),
  reverse(Caminho2, Caminho2Aux),
  tail(Caminho2Aux, Caminho2Final),

  append(Caminho1Final, Caminho2Final, CaminhoFinal),
  CustoFinal is CustoIda + Custo2.



%%-----------------------------------------------------------
% "Circuito IDS com lista de packages"                       |
% circuitoIDSL : packageIDs list, Caminho, Kms               |
% usage : circuitoIDSL([packagesID],Caminho,Distancia).      |
%%-----------------------------------------------------------


circuitoIDSL([PackageID | Resto],Caminho,Km) :-
  
  list_addresses([PackageID | Resto],List),
  idslAux((escolaEngenharia1, uni_centro),List,Caminho,Km).



%%-------------------------------------------------------------------------------------------------------------------
% "Iterative Deepening Search, Indicador de produtividade"                                                           |
% indicadorProdutividadeBFS : PackageID, EcoTransportation, Caminho, Ida, Km                                         |
% usage : indicadorProdutividadeIDS(packageid, (rua, freguesia), EcoT, Cam, CustoIda, CustoTotal).                   |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeIDS(PackageID, EcoTransportation, Caminho, Ida, Km) :-
    
    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    melhorSolucaoIDS((Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).





%%-------------------------------------------------------------
% "CircuitoGreedy"                                             |
% CircuitoGreedy : Inicio, Caminho, KmIda, Km                  |
% usage : circuitoGreedy((rua, freguesia), Cam, Kmida, Km).    |
%                                                              |
% Circuito completo de ida e volta até UM ponto de entrega     |
%%-------------------------------------------------------------

circuitoGreedy(PontoEntrega, Caminho, KmIda, Km) :-

%%- procura o caminho desde o ponto de entrega até à base

  get_gulosa(PontoEntrega, Cam, KmIda),

  Km is (KmIda * 2),

%%- aproveita a cauda dessa lista para fazer a volta

  tail(Cam, Cam1),

%%- inverte o caminho para representar a ida

  reverse(Cam, Ida),

%%- dá append da ida (caminho original invertido) com a cauda (caminho original)

  append(Ida, Cam1, Caminho).



circuitoGreedyList([PackageID | Resto], Caminho, Km) :-

  list_addresses([PackageID | Resto], List),

  gulosaList((escolaEngenharia1, uni_centro), List, Caminho, Km).



%%-------------------------------------------------------------------------------------------------------------------
% "Greedy Search"                                                                                                    |
% indicadorProdutividadeGreedy : PackageID, EcoTransportation, Caminho, Ida, Km                                      |
% usage : indicadorProdutividadeGreedy(packageid, EcoT, Cam, CustoIda, CustoTotal).                                  |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeGreedy(PackageID, EcoTransportation, Caminho, Ida, Km) :-

    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    circuitoGreedy( (Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).




%%-------------------------------------------------------------
% "Circuito A* (A estrela)"                                    |
% CircuitoAEstrela : Inicio, Caminho, KmIda, Km                |
% usage : circuitoAEstrela((rua, freguesia), Cam, Kmida, Km).  |
%                                                              |
% Circuito completo de ida e volta até UM ponto de entrega     |
%%-------------------------------------------------------------

circuitoAEstrela(Inicio, Caminho, KmIda, Km) :-

  get_AEstrela(Inicio, Cam, KmIda),

  Km is KmIda * 2,

  tail(Cam, Cam1),
  
  reverse(Cam, Ida),
  
  append(Ida, Cam1, Caminho).
  
  
%%----------------------------------------------------------
% "A* com lista de packages"                                |
% circuitoAEstrelaL : List of PackageID, Caminho, Distancia |
% usage : circuitoDFSL([packageID], Caminho, Distancia).    |
%%----------------------------------------------------------
  
circuitoAEstrelaL([PackageID | Resto],Caminho,Km) :-
  
  list_addresses([PackageID | Resto],List),
  
  aEstrelaList((escolaEngenharia1, uni_centro), List, Caminho, Km).


%%-----------------------------------------------------------------------------------------------------------------------
% "A Estrela, Indicador de produtividade"                                                                                |
% indicadorProdutividadeAEstrela : PackageID, Transport, Caminho, KmIda, KmTotais                                        |
% usage : indicadorProdutividadeAEstrela(packageid, EcoTransport, Cam, Kmida, Km)                                        |
%                                                                                                                        |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar     |
%%-----------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeAEstrela(PackageID, EcoTransportation, Caminho, Ida, Km) :-

    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    circuitoAEstrela( (Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).


%%---------------------------------------------------------------------------------------------
% Info : Package, Estafeta, Caminho, CustoIda, Custo                                           |
% A partir de um packageID, consegue-se consultar informação sobre o estafeta que o entregou,  |
% o caminho que este utilizou, assim como o custo de ida e o custo total (km percorridos)      |
% usage : info(101, E, Cam, Cida, Ctotal).                                                     |
%%---------------------------------------------------------------------------------------------

info(PackageID, Estafeta, Caminho, CustoIda, Custo) :-

    findall((estafeta(A,B,C)), (estafeta(A,B,C), member(PackageID, C)), Estafeta),

    indicadorProdutividadeBFS(PackageID, _, Caminho, CustoIda, Custo).


%%---------------------------------------------------------------------------------------------------------------
% ListagemCircuitos : Listagem                                                                                   |
% Após recolher todos os estafetas presentes na base de conhecimento, organiza cada circuito                     |
% que efetuou agrupando, em triplos, o n.º de packages levados no circuito, os addresses que teve                |
% de visitar, assim como o algoritmo utilizado para calcular o mesmo : (Npackages, [Addresses], Algoritmo)       |
% Por fim, utiliza o algoritmo de ordenação merge_sort aplicado a triplos, para devolver uma lista ordenada      |
% do circuito que mais vezes foi utilizado para entregar encomendas, ao que menos foi utilizado.                 |
%%---------------------------------------------------------------------------------------------------------------

listagemCircuitos() :-

  findall((C,D), estafeta(_,_,C,D), Estafetas),

  constroiCaminho(Estafetas, L),

  merge_sort(L, SortedList),

  reverse(SortedList, Listagem),

  imprimirListagem(Listagem).



reproduzCaminho(Address, dfs, Caminho, Km) :-

  melhorSolucaoDFS(Address, Caminho, _, Km).

reproduzCaminho(Address, bfs, Caminho, Km) :-

  melhorSolucaoBFS(Address, Caminho, _, Km).

reproduzCaminho(Address, ids, Caminho, Km) :-

  melhorSolucaoIDS(Address, Caminho, _, Km).

reproduzCaminho(Address, astar, Caminho, Km) :-

  circuitoAEstrela(Address, Caminho, _, Km).

reproduzCaminho(Address, greedy, Caminho, Km) :-

  circuitoGreedy(Address, Caminho, _, Km).




imprimirListagem([]).


imprimirListagem([(Npackages, [Address], Algoritmo) | Resto]) :-

  write("Número de encomendas entregues : "),write(Npackages),nl,write(", Cam = "),

  reproduzCaminho(Address, Algoritmo, Caminho, Km),

  write(Caminho),nl,write(", KM = "),write(Km),nl,nl,

  imprimirListagem(Resto).


%%---------------------------------------------------------------
% Merge Sort, auxiliar de ordenação de uma lista com triplos     |
%%---------------------------------------------------------------

merge_sort([],[]).     % Uma lista vazia já se encontra ordenada

merge_sort([X],[X]).   % Uma linha que contenha um único elemento já se encontra ordenada

merge_sort(List,Sorted):-

  List=[_,_|_], split(List,L1,L2),     % Lista com pelo menos 2 elementos é dividida em dois

  merge_sort(L1,Sorted1), merge_sort(L2,Sorted2),  % then each part is sorted

  merge(Sorted1, Sorted2, Sorted).                  % and sorted parts are merged


%%----------------
% Merge two lists |
%%----------------

merge([],L,L).

merge(L,[],L):- L\= [].

merge([(X,A,B)|T1],[(Y,C,D)|T2],[(X,A,B)|T]):- X =< Y, merge(T1,[(Y,C,D)|T2],T).

merge([(X,C,D)|T1],[(Y,A,B)|T2],[(Y,A,B)|T]):- X > Y, merge([(X,C,D)|T1],T2,T).

%%--------
% Split   |
%%--------

split([], [], []).
split([A], [A], []).
split([A, B | C], [A | D], [B | E]) :-

  split(C, D, E).



%%--------------------------------------------------------------------------------------------------
% ConstroiCaminho : [ (ListaPackages, Algoritmo) ], [ (Npackages, Address) ]                        |
%                                                                                                   |
% Recebendo uma lista de packages que um estafeta possa ter levado, e o algoritmo associado         |
% a esse circuito que efetuou, consegue agrupar numa lista de pares o n.º de packages associado     |
% a cada address que tenha sido entregue                                                            |
%%---------------------------------------------------------------------------------------------------


constroiCaminho([], []).

constroiCaminho( [ (ListaPackages, Algoritmo) | Resto ], Addresses) :-

  getPackagesAddresses(ListaPackages, (N, Ruas)),

  constroiCaminho(Resto, Addresses2),

  updateTripletList((N,Ruas, Algoritmo), Addresses2, Addresses), !.



%%-------------------------------------------------------------------------------
% GetPackagesAddresses : PackageIDS, NumeroDePackages, ListaDeAddresses          |
% Dada uma lista de id´s de packages, devolve o número de packages dessa lista   |
% e ainda a lista com os addresses associados a cada um                          |
%%-------------------------------------------------------------------------------

getPackagesAddresses( [PackageID], (Number, [(Rua,Freguesia)])) :-

  package(PackageID, _, _, _, address(Rua,Freguesia), _, _),

  Number is 1 .


getPackagesAddresses([PackageID | Resto], (Number, Ruas)) :-

  package(PackageID, _, _, _, address(Rua,Freguesia), _, _),

  This = [(Rua,Freguesia)],

  getPackagesAddresses(Resto, (Number1, That)),

  append(This, That, Ruas),

  Number is 1 + Number1.





%%---------------------------------------------------------------------------------------
% UpdateTripletList : (Number, Object1, Object2), List, UpdatedList                      |
% usage : updatePairList( (1,a,aa), [(1,a,aa),(2,b,c)], [(2,a,a), (2,b,c)]).             |
%                                                                                        |
% Dada uma lista de triplos (tem de estar sempre inicializada), agrupa os elementos      |
% iguais e adiciona os membros novos                                                     |
%%---------------------------------------------------------------------------------------


updateTripletList( (N,O), [], [(N,O)]).

updateTripletList( (Number, Object1, Object2), [ (N, Object1, Object2) | Resto], ListaAtualizada) :-

  Object1 = Object1,
  Object2 = Object2,

  Number1 is Number + N,

  ListaAtualizada = [ (Number1, Object1, Object2) | Resto ].


updateTripletList( (Number, Object11, Object12), [ (A, Object21, Object22) | Resto], ListaAtualizada) :-

  (Object11 \== Object21 ; Object12 \== Object22),

  updateTripletList( (Number, Object11, Object12), Resto, Lista),

  append( [(A,Object21, Object22)], Lista, ListaAtualizada).