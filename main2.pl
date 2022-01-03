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

indicadorProdutividadeBFS(PackageID, (Rua,Freguesia), EcoTransportation, Caminho, Km) :-

    melhorSolucaoBFS((Rua, Freguesia), Caminho, Km),

    getAllPossibleTransports(PackageID, Km, List),

    moreEcologicalTransportation(List, EcoTransportation).

%%- Indicador de produtividade que devolve o melhor caminho associado ao transporte mais ecológico que este pode tomar
%%- Iterative deepening search

indicadorProdutividadeIDS(PackageID, (Rua,Freguesia), EcoTransportation, Caminho, Km) :-

    melhorSolucaoIDS((Rua, Freguesia), Caminho, Km),

    getAllPossibleTransports(PackageID, Km, List),

    moreEcologicalTransportation(List, EcoTransportation).





%%- Dado um número, articula-o na parte das unidades e das casas decimais
%%- usage : getDecimalPart(1.234, U, D).

getDecimalPart(Number, Unity, Decimal) :-

    Unity is floor(Number),

    Decimal is Number - Unity.


%%- Converte um número na sua representação em Horas:Minutos:Segundos
%%- (Não respeita os limites do horário de um dia)
%%- usage : converterHoras(1.234, Answer).

converterHoras(Hora, hour(H,M,S)) :-
    
    Hora >= 0,

    %%- H é a hora, em unidades
    H is floor(Hora),

    %%- Parte decimal da 1.ª
    Decimal1 is (Hora - H),
    
    %%- M são os minutos, em unidade
    M is floor(Decimal1 * 60),

    %%- Parte decimal da 2.ª
    Decimal2 is ((Decimal1 * 60) - M),

    %%- S são os segundos, em unidades
    S is floor(Decimal2 * 60).




info(PackageID, Estafeta, Caminho, Custo) :-

    package(PackageID, _, _, _, address(Rua, Freguesia), _, _),

    findall((estafeta(A,B,C)), (estafeta(A,B,C), member(PackageID, C)), Estafeta),

    indicadorProdutividadeBFS(PackageID, (Rua, Freguesia), _, Caminho, Custo).