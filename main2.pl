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
 



%%-------------------------------------------------------------------------------------------------------------------
% "Depth first search"                                                                                               |
% indicadorProdutividadeDFS : PackageID, (Rua,Freguesia), EcoTransportation, Caminho, Ida, Km                        |
% usage : indicadorProdutividadeDFS(packageid, (rua, freguesia), EcoT, Cam, CustoIda, CustoTotal).                   |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeDFS(PackageID, EcoTransportation, Caminho, Ida, Km) :-
    
    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    melhorSolucaoDFS((Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).



%%-------------------------------------------------------------------------------------------------------------------
% "Breadth first search"                                                                                             |
% indicadorProdutividadeBFS : PackageID, (Rua,Freguesia), EcoTransportation, Caminho, Ida, Km                        |
% usage : indicadorProdutividadeBFS(packageid, (rua, freguesia), EcoT, Cam, CustoIda, CustoTotal).                   |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeBFS(PackageID, EcoTransportation, Caminho, Ida, Km) :-
    
    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),
    
    melhorSolucaoBFS((Rua, Freguesia), Caminho, Ida, Km),

    getAllPossibleTransports(PackageID, Ida, List),

    moreEcologicalTransportation(List, EcoTransportation).



%%-------------------------------------------------------------------------------------------------------------------
% "Iterative Deepening Search"                                                                                       |
% indicadorProdutividadeBFS : PackageID, (Rua,Freguesia), EcoTransportation, Caminho, Ida, Km                        |
% usage : indicadorProdutividadeIDS(packageid, (rua, freguesia), EcoT, Cam, CustoIda, CustoTotal).                   |
%                                                                                                                    |
% Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar |
%%-------------------------------------------------------------------------------------------------------------------

indicadorProdutividadeIDS(PackageID, EcoTransportation, Caminho, Ida, Km) :-
    
    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    melhorSolucaoIDS((Rua, Freguesia), Caminho, Ida, Km),

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