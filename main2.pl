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
 
% ---------------------------------------------------------


hour(A,B,C) :-

    A >= 0, A =< 23,
    B >= 0, B =< 59,
    C >= 0, C =< 59.


%%------------------------------------------------------------------------------


%%- Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar
%%. Depth first search

indicadorProdutividadeDFS(PackageID, (Rua,Freguesia), EcoTransportation, Caminho, Km) :-

    melhorSolucaoDFS((Rua, Freguesia), Caminho, Km),

    getAllPossibleTransports(PackageID, Km, List),

    moreEcologicalTransportation(List, EcoTransportation).

%%- Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar
%%- Breadth first search

indicadorProdutividadeBFS(PackageID, (Rua,Freguesia), EcoTransportation, Km) :-

    melhorSolucaoBFS((Rua, Freguesia), _, Km),

    getAllPossibleTransports(PackageID, Km, List),

    moreEcologicalTransportation(List, EcoTransportation).

%%- Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar
%%- Iterative deepening search

indicadorProdutividadeIDS(PackageID, (Rua,Freguesia), EcoTransportation, Km) :-

    melhorSolucaoIDS((Rua, Freguesia), _, Km),

    getAllPossibleTransports(PackageID, Km, List),

    moreEcologicalTransportation(List, EcoTransportation).