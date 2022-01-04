%----------------------
% File written by------
% ---------------G28---
%----------------------
% cls :- write('\e[H\e[2J').
% % make. to reload

% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').


g( grafo(

  [
      % cada nodo é um ADDRESS, address : Rua, Freguesia

  address( complexoPedagogico1, uni_este ),       % 1
  address( complexoPedagogico2, uni_sul ),        % 2
  address( complexoPedagogico3, uni_centro),      % 3
  address( biblioteca, uni_sul),                  % 4
  address( institutoLetras, uni_sul),             % 5
  address( escolaCiencias, uni_centro),           % 6
  address( escolaEngenharia1, uni_centro),        % 7
  address( escolaEconomia, uni_centro),           % 8
  address( complexoDesportivo, uni_este),         % 9
  address( servicosTecnicos, uni_este),           % 10
  address( cantina, uni_norte),                   % 11
  address( acaoSocial, uni_norte),                % 12
  address( educacao, uni_oeste),                  % 13
  address( psicologia, uni_oeste),                % 14
  address( cienciasSociais, uni_oeste),           % 15
  address( escolaEngenharia2, uni_oeste),         % 16
  address( escolaDireito, uni_oeste),             % 17
  address( bioSustentabilidade, uni_oeste),       % 18
  address( medicina, olimpo)                      % 19
  
  ],


      % cada aresta é um par ({RUA, FREGUESIA}, {RUA,FREGUESIA}), acompanhada do peso
      % entre esses dois pontos 


    [ % 7 -> 9 = 16
    aresta(  (escolaEngenharia1, uni_centro), (complexoDesportivo, uni_este), 16),
      % 7 -> 6 = 5 
    aresta(  (escolaEngenharia1, uni_centro), (escolaCiencias, uni_centro), 5),
      %  7 -> 8 = 7
    aresta(  (escolaEngenharia1, uni_centro), (escolaEconomia, uni_centro), 7),
      %  7 -> 3 = 6
    aresta(  (escolaEngenharia1, uni_centro), (complexoPedagogico3, uni_centro), 6),
      % 9 -> 10 = 9 
    aresta(  (complexoDesportivo, uni_este), (servicosTecnicos, uni_este), 9),
      % 9 -> 1 = 11
    aresta(  (complexoDesportivo, uni_este), (complexoPedagogico1, uni_este), 11),
      % 10 -> 1 = 15
    aresta(  (servicosTecnicos, uni_este), (complexoPedagogico1, uni_este), 15),
      % 1 -> 4 = 2
    aresta(  (complexoPedagogico1, uni_este), (biblioteca, uni_sul), 2),
      % 6 -> 5 = 3
    aresta(  (escolaCiencias, uni_centro), (institutoLetras, uni_sul), 3),
      % 5 -> 4 = 1
    aresta(  (intitutoLetras, uni_sul), (biblioteca, uni_sul), 1),
      % 5 -> 2 = 2
    aresta(  (institutoLetras, uni_sul), (complexoPedagogico2, uni_sul), 2),
      % 4 -> 2 = 3
    aresta(  (biblioteca, uni_sul), (complexoPedagogico2, uni_sul), 3),
      % 2 -> 3 = 6
    aresta(  (complexoPedagogico2, uni_sul), (complexoPedagogico3, uni_centro), 6),
      % 2 -> 13 = 22
    aresta(  (complexoPedagogico2, uni_sul), (educacao, uni_oeste), 22),
      % 2 -> 15 = 23
    aresta(  (complexoPedagogico2, uni_sul), (cienciasSociais, uni_oeste), 23),
      % 13 -> 14 = 4
    aresta(  (educacao, uni_oeste), (psicologia, uni_oeste), 4),
      % 14 -> 15 = 5
    aresta(  (psicologia, uni_oeste), (cienciasSociais, uni_oeste), 5),
      % 15 -> 16 = 3
    aresta(  (cienciasSociais, uni_oeste), (escolaEngenharia2, uni_oeste), 3),
      % 15 -> 17 = 7
    aresta(  (cienciasSociais, uni_oeste), (escolaDireito, uni_oeste), 7),
      % 16 -> 17 = 5
    aresta(  (escolaEngenharia2, uni_oeste), (escolaDireito, uni_oeste), 5),
      % 17 -> 18 = 2
    aresta(  (escolaDireito, uni_oeste), (bioSustentabilidade, uni_oeste), 2),
      % 17 -> 8 = 16
    aresta(  (escolaDireito, uni_oeste), (escolaEconomia, uni_centro), 16),
      % 8 -> 11 = 3
    aresta(  (escolaEconomia, uni_centro), (cantina, uni_norte), 3),
      % 11 -> 12 = 2
    aresta(  (cantina, uni_norte), (acaoSocial, uni_norte), 2),
      % 12 -> 19 = 28
    aresta(  (acaoSocial, uni_norte), (medicina, olimpo), 28),
      % 19 -> 18 = 25
    aresta(  (medicina, olimpo), (bioSustentabilidade, uni_oeste), 25)
    ]
 )).


adjacentes(A,B,C, grafo(_,Arestas)) :- 
                
                member(aresta(A,B,C) , Arestas).

adjacentes(A,B,C, grafo(_,Arestas)) :-
                
                member(aresta(B,A,C), Arestas).


% usage: g(grafo(A,B)), adjacentes((medicina,olimpo), (bioSustentabilidade, uni_oeste), 16, grafo(_,B)).





%-----------------------------------------------------------------

move(  (escolaEngenharia1, uni_centro), (complexoDesportivo, uni_este), 16).
move(  (escolaEngenharia1, uni_centro), (escolaCiencias, uni_centro), 5).
move(  (escolaEngenharia1, uni_centro), (escolaEconomia, uni_centro), 7).
move(  (escolaEngenharia1, uni_centro), (complexoPedagogico3, uni_centro), 6).
move(  (complexoDesportivo, uni_este), (servicosTecnicos, uni_este), 9).
move(  (complexoDesportivo, uni_este), (complexoPedagogico1, uni_este), 11).
move(  (servicosTecnicos, uni_este), (complexoPedagogico1, uni_este), 15).
move(  (complexoPedagogico1, uni_este), (biblioteca, uni_sul), 2).
move(  (escolaCiencias, uni_centro), (institutoLetras, uni_sul), 3).
move(  (institutoLetras, uni_sul), (biblioteca, uni_sul), 1).
move(  (institutoLetras, uni_sul), (complexoPedagogico2, uni_sul), 2).
move(  (biblioteca, uni_sul), (complexoPedagogico2, uni_sul), 3).
move(  (complexoPedagogico2, uni_sul), (complexoPedagogico3, uni_centro), 6).
move(  (complexoPedagogico2, uni_sul), (educacao, uni_oeste), 22).
move(  (complexoPedagogico2, uni_sul), (cienciasSociais, uni_oeste), 23).
move(  (educacao, uni_oeste), (psicologia, uni_oeste), 4).
move(  (psicologia, uni_oeste), (cienciasSociais, uni_oeste), 5).
move(  (cienciasSociais, uni_oeste), (escolaEngenharia2, uni_oeste), 3).
move(  (cienciasSociais, uni_oeste), (escolaDireito, uni_oeste), 7).
move(  (escolaEngenharia2, uni_oeste), (escolaDireito, uni_oeste), 5).
move(  (escolaDireito, uni_oeste), (bioSustentabilidade, uni_oeste), 2).
move(  (escolaDireito, uni_oeste), (escolaEconomia, uni_centro), 16).
move(  (escolaEconomia, uni_centro), (cantina, uni_norte), 3).
move(  (cantina, uni_norte), (acaoSocial, uni_norte), 2).
move(  (acaoSocial, uni_norte), (medicina, olimpo), 28).
move(  (medicina, olimpo), (bioSustentabilidade, uni_oeste), 25).


%%--------------------------------------------
% Verifica se dois Addresses estão conectados |
%%-------------------------------------------

connected( A, B, C ) :- move( A, B, C).
connected( A, B, C ) :- move( B, A, C).


%----- "DEPTH FIRST SEARCH" -----------------------------------------------------------------------------

%%------------------------------------------------------------------
% CircuitoDFS : (Rua, Freguesia), Caminho, CustoIda, Custo)         |
% usage : circuitoDFS((rua, freguesia), Cam, CustoIda, CustoFinal). |
%%------------------------------------------------------------------

circuitoDFS(PontoEntrega, CaminhoFinal, CustoIDA, CustoFinal) :-

  ida(PontoEntrega, Caminho1Aux, CustoIDA),

  volta(PontoEntrega, Caminho2Aux, Custo2),

  tail(Caminho2Aux, Caminho2),

  append(Caminho1Aux, Caminho2, CaminhoFinal),

  CustoFinal is CustoIDA + Custo2.


%%-----------------------------------------------------------------------------
% Ida : PontoEntrega, Caminho, Custo                                           |
% usage : ida( (rua,freguesia), Cam, Custo).                                   |
% Realiza uma procura utilizando o depth first search de um ponto pré-definido |
% (definido no corpo da regra) até um ponto final de entrega                   |
%%-----------------------------------------------------------------------------

ida(PontoEntrega, CaminhoFinal, Custo) :-

  dfs([], (escolaEngenharia1, uni_centro), Caminho, Custo, PontoEntrega),
  reverse(Caminho, CaminhoFinal).

%%----------------------------------------------------------------------
% Volta : PontoInicial, Caminho, Custo                                  |
% usage : volta( (rua,freguesia), Cam, Custo).                          |
% Realiza uma procura utilizando o depth first search de um ponto dado  |
% até um ponto final de entrega (definido no corpo da regra)            |
%%----------------------------------------------------------------------

volta(Inicio, Caminho, Custo) :-

  dfs([], Inicio, CaminhoAux, Custo, (escolaEngenharia1, uni_centro)),
  reverse(CaminhoAux, Caminho).


%%------------------
% "Algoritmo DFS"   |
%%------------------

dfs(Visitados, PontoEntrega, [PontoEntrega | Visitados], 0, PontoEntrega).

dfs(Visitados, Nodo, Caminho, Custo, PontoEntrega) :-

  connected(Nodo, NextNodo, C1),
  \+member(NextNodo, Visitados),
  dfs([Nodo | Visitados], NextNodo, Caminho, C2, PontoEntrega),

  Custo is C1 + C2.



  %----- "BREADTH FIRST SEARCH" -----------------------------------------------------------------------------
 
%%------------------------------------------------------------------
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


%%-----------------------------------------------------------------------------
% BFS2 : a partir de um destino, procura um caminho por breadth first search a |
% partir de um ponto inicial pré-definido (no corpo da regra)                  |
%------------------------------------------------------------------------------

%- Condição final, onde a HEAD é o destino final

bfs2(Dest,[[Dest|T]|_],Cam):-

  reverse([Dest|T],Cam).


bfs2(Dest,[LA|Outros],Cam):-
  
  LA=[Act|_],
  
  findall([X|LA],
  
  (Dest\==Act,connected(Act,X, _),\+member(X,LA)), 

  Novos),

  append(Outros, Novos, Todos),
  
  bfs2(Dest,Todos,Cam).


%%-------------------------------------------------------
% Distance : Caminho , Distancia                         |
% Calcula a distância entre uma lista de moradas/arestas |
% usage : distance([lista], Km).                         |
%%-------------------------------------------------------

distance([Inicio, Next], Km) :- connected(Inicio, Next, Km).

distance([Inicio, Next | Resto], Km) :-

    connected(Inicio, Next, Km1),
    distance([Next | Resto], Km2),
    Km is Km1+Km2. 



  %----- "Iterative Deepening Search" -----------------------------------------------------------------------------

/*

Limitations of this approach :
- Bratko´s implementation is quite elegant, and it will return a solution if one exists.
- Returns all alternative solutions on backtracking.
- However, if there are no solutions (or no more solutions on backtracking), search
does not terminate, even if the state space is finite.
- Can´t be used to exhaustively enumerate all solutions (e.g., with all solutions
  predicates).
- Brantko sees the nodes more as States, than an actual node.

*/

%%--------------------------------------------------------------------
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


%%-------------------------------------------------------------------------
% Path : PontoInicial, PontoEntrega, Custo, Caminho                        |
% procura um caminho a partir de um ponto inicial até um ponto de entrega, |
% a partir das arestas definidas/disponíveis                               |
% usage : path( (rua, freguesia), (rua,freguesia), Custo, Caminho).        |
%%-------------------------------------------------------------------------

path(PontoEntrega, PontoEntrega, 0, [PontoEntrega]).

path(Inicio, Fim, Custo, [Fim | Path]) :-

  path(Inicio, Penultimo, C1, Path),

  connected(Penultimo, Fim, C2),

  \+member(Fim, Path),

  Custo is C1+C2.



/*
------ Depth limited Search (teste)

dls(Last, Last, [Last], D):-

  D >= 0.

dls(First, Last, [First | Resto], D) :-

  D > 0,
  connected(First, Algo, _),
  D1 is D - 1,
  dls(Algo, Last, Resto, D1).

*/




%%---------------------------------------------------------------------------------------------
%  "MELHOR SOLUCAO : DEPTH FIRST SEARCH : (PontoEntrega, Caminho, CustoIda, CustoTotal)"       |
%%---------------------------------------------------------------------------------------------

melhorSolucaoDFS(PontoEntrega, MelhorCaminho, MelhorIda, MelhorCusto) :-

  findall((Caminho, Ida, Custo),

          (circuitoDFS(PontoEntrega, Caminho, Ida, Custo)),
          List),

  menorTL(List, (MelhorCaminho, MelhorIda,  MelhorCusto)).


%%---------------------------------------------------------------------------------------------
%- "MELHOR SOLUCAO : BREADTH FIRST SEARCH : (PontoEntrega, Caminho, CustoIda, CustoTotal)"     |
%%---------------------------------------------------------------------------------------------

melhorSolucaoBFS(PontoEntrega, MelhorCaminho, MelhorIda, MelhorCusto) :-

  findall((Caminho, Ida, Custo),

          (circuitoBFS(PontoEntrega, Caminho, Ida,Custo)),
          List),
  menorTL(List, (MelhorCaminho, MelhorIda, MelhorCusto)).


%%----------------------------------------------------------------------------------------------
% "MELHOR SOLUCAO : ITERATIVE DEEPENING SEARCH : (PontoEntrega, Caminho, CustoIda, CustoTotal)" |
% Neste algoritmo, fazer backtracking faz com que o programa fique em loop infinito             |
% A melhor solução vai ser sempre a primeira                                                    |
%%----------------------------------------------------------------------------------------------

melhorSolucaoIDS(PontoEntrega, MelhorCaminho, Ida, MelhorCusto) :-

  circuitoIDS(PontoEntrega, MelhorCaminho, Ida, MelhorCusto),
  !.