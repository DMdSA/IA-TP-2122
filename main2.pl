%----------------------
% File written by------
% ---------------G28---
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload

% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').
:- consult('grafo.pl').
:- consult('IndicadoresProdutividade.pl').
 



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
% "DFS com lista de packages"
% circuitoDFSL : List of PackageID, Caminho, Distancia   |
% usage : circuitoDFSL([packageID], Caminho, Distancia). |
%%-------------------------------------------------------

circuitoDFSL([PackageID | Resto],Caminho,Kms) :-
  list_addresses([PackageID | Resto],List),
  dfsl((escolaEngenharia1,uni_centro),List,Caminho,Kms).



%%-------------------------------------------------------------------------------------------------------------------
% "Depth first search, Indicador de produtividade"                                                                                               |
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
% "Breadth first search, Indicador de produtividade"                                                                                             |
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
% "Circuito Breadth first search, lista de packages"
% circuitoBFSL : packageIDs list, Caminho, Kms               |
% usage : circuitoBFSL([packagesID],Caminho,Distancia).      |
% "comment"
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


%%-
% "comment"
%%-

circuitoIDSL([PackageID | Resto],Caminho,Km) :-
  
  list_addresses([PackageID | Resto],List),
  idslAux((escolaEngenharia1, uni_centro),List,Caminho,Km).



%%-------------------------------------------------------------------------------------------------------------------
% "Iterative Deepening Search, Indicador de produtividade"                                                                                       |
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





%%------------------------------
% CircuitoGreedy
% CircuitoGreedy : PontoEntrega, Caminho
% "comment"
%%------------------------------

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


%%-----------------------------------------------------------------------------------------------------------------------
% "A Estrela, Indicador de produtividade"                                                                                |
% indicadorProdutividadeAEstrela : PackageID, Transport, Caminho, KmIda, KmTotais                                        |
% usage : indicadorProdutividadeAEstrela(packageid, EcoTransport, Cam, Kmida, Km)                                        |
%                                                                                                                        |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar     |
%%-----------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeGreedy(PackageID, EcoTransportation, Caminho, Ida, Km) :-

    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    circuitoAEstrela( (Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).










% n.o.t. u.s.e.d. y.e.t.
%%---------------------------------------------------------------------------------------------
% Info : Package, Estafeta, Caminho, CustoIda, Custo                                           |
% A partir de um packageID, consegue-se consultar informação sobre o estafeta que o entregou,  |
% o caminho que este utilizou, assim como o custo de ida e o custo total (km percorridos)      |
% usage : info(101, E, Cam, Cida, Ctotal).                                                     |
%%---------------------------------------------------------------------------------------------

info(PackageID, Estafeta, Caminho, CustoIda, Custo) :-

    findall((estafeta(A,B,C)), (estafeta(A,B,C), member(PackageID, C)), Estafeta),

    indicadorProdutividadeBFS(PackageID, _, Caminho, CustoIda, Custo).





