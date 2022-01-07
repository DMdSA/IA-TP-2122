%----------------------
% File written by------
% ---------------G28---
%----------------------
% cls :- write('\e[H\e[2J').
% % make. to reload

% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').
:- consult('Util.pl').
:- consult('IndicadoresProdutividade.pl').


g( grafo(

  [
      % cada nodo é um ADDRESS, address : Rua, Freguesia

  address( complexoPedagogico1, uni_este ),       % A 1
  address( complexoPedagogico2, uni_sul ),        % B 2
  address( complexoPedagogico3, uni_centro),      % C 3
  address( biblioteca, uni_sul),                  % D 4
  address( institutoLetras, uni_sul),             % E 5
  address( escolaCiencias, uni_centro),           % F 6
  address( escolaEngenharia1, uni_centro),        % G 7
  address( escolaEconomia, uni_centro),           % H 8
  address( complexoDesportivo, uni_este),         % I 9
  address( servicosTecnicos, uni_este),           % J 10
  address( cantina, uni_norte),                   % K 11
  address( acaoSocial, uni_norte),                % L 12
  address( educacao, uni_oeste),                  % M 13
  address( escolaPsicologia, uni_oeste),          % N 14
  address( cienciasSociais, uni_oeste),           % O 15
  address( escolaEngenharia2, uni_oeste),         % P 16
  address( escolaDireito, uni_oeste),             % Q 17
  address( bioSustentabilidade, uni_oeste),       % R 18
  address( medicina, olimpo)                      % S 19
  
  ],

      % cada aresta é um par ({RUA, FREGUESIA}, {RUA,FREGUESIA}), acompanhada do peso
      % entre esses dois pontos

  [

  % G -> K = 2
  aresta(  (escolaEngenharia1, uni_centro), (cantina, uni_norte), 2),
  % K -> H = 2
  aresta( (cantina, uni_norte), ( escolaEconomia, uni_centro), 2),
  % H -> R = 4
  aresta( (escolaEconomia, uni_centro), (bioSustentabilidade, uni_norte), 4), 
  % H -> S = 8
  aresta( (escolaEconomia, uni_centro), ( medicina, olimpo), 8),
  % R -> S = 7
  aresta( (bioSustentabilidade, uni_norte), (medicina, olimpo), 7),
  % R -> Q = 1
  aresta( (bioSustentabilidade, uni_norte), (escolaDireito, uni_oeste), 1),
  % Q -> P = 2
  aresta( (escolaDireito, uni_oeste), (escolaEngenharia2, uni_oeste), 2),
  % P -> O = 1
  aresta( (escolaEngenharia2, uni_oeste), (cienciasSociais, uni_oeste), 1),
  % O -> N = 2
  aresta( (cienciasSociais, uni_oeste), (escolaPsicologia, uni_oeste), 2), 
  % N -> M = 2
  aresta( (escolaPsicologia, uni_oeste), (educacao, uni_oeste), 2),
  % M -> B = 5
  aresta( (educacao, uni_oeste), (complexoPedagogico2, uni_sul), 5),
  % B -> H = 5
  aresta( (complexoPedagogico2, uni_sul), (escolaEconomia, uni_centro), 5),
  % B -> G = 4 
  aresta( (complexoPedagogico2, uni_sul), (escolaEngenharia1, uni_centro), 4),
  % B -> C = 3
  aresta( (complexoPedagogico2, uni_sul), (escolaPedagogico3, uni_centro), 3),
  % C -> G = 2
  aresta( (complexoPedagogico3, uni_centro), (escolaEngenharia1, uni_centro), 2),
  % B -> D = 2
  aresta( (complexoPedagogico2, uni_centro), (biblioteca, uni_sul), 2),
  % H -> L = 2
  aresta(  (escolaEconomia, uni_centro), (acaoSocial, uni_norte), 2),
  % K -> L = 3
  aresta( (cantina, uni_norte), (acaoSocial, uni_norte), 3),
  % L -> S = 5 
  aresta( (acaoSocial, uni_norte), (medicina, olimpo), 5),
  % G -> F = 3
  aresta( (escolaEngenharia1, uni_centro), (escolaCiencias, uni_centro), 3),
  % D -> E = 1
  aresta( (biblioteca, uni_sul), (institutoLetras, uni_sul), 1),
  % E -> F = 2
  aresta( (institutoLetras, uni_sul), (escolaCiencias, uni_centro), 2),
  % A -> I = 6
  aresta( ( complexoPedagogico1, uni_este ), (complexoDesportivo, uni_este), 6), 
  % A -> F = 3
  aresta( (complexoPedagogico1, uni_este), (escolaCiencias, uni_centro), 3),
  % I -> J = 2
  aresta( (complexoDesportivo, uni_este), (servicosTecnicos, uni_este), 2),
  % K -> J = 5
  aresta( (cantina, uni_norte), (servicosTecnicos, uni_este), 5),
  % R -> O = 2
  aresta( (bioSustentabilidade, uni_oeste), (cienciasSociais, uni_oeste), 2),
  % O -> M = 3
  aresta( (cienciasSociais, uni_oeste), (educacao, uni_oeste), 3),
  % O -> C = 6
  aresta( (cienciasSociais, uni_oeste), (complexoPedagogico3, uni_centro), 6)

  ]
  )).


adjacentes(A,B,C, grafo(_,Arestas)) :- 
                
                member(aresta(A,B,C) , Arestas).

adjacentes(A,B,C, grafo(_,Arestas)) :-
                
                member(aresta(B,A,C), Arestas).


% usage: g(grafo(A,B)), adjacentes((medicina,olimpo), (bioSustentabilidade, uni_oeste), 16, grafo(_,B)).





%-----------------------------------------------------------------

move( (escolaEngenharia1, uni_centro), (cantina, uni_norte), 2).
move( (cantina, uni_norte), ( escolaEconomia, uni_centro), 2).
move( (escolaEconomia, uni_centro), (bioSustentabilidade, uni_norte), 4).
move( (escolaEconomia, uni_centro), ( medicina, olimpo), 8).
move( (bioSustentabilidade, uni_norte), (medicina, olimpo), 7).
move( (bioSustentabilidade, uni_norte), (escolaDireito, uni_oeste), 1).
move( (escolaDireito, uni_oeste), (escolaEngenharia2, uni_oeste), 2).
move( (escolaEngenharia2, uni_oeste), (cienciasSociais, uni_oeste), 1).
move( (cienciasSociais, uni_oeste), (escolaPsicologia, uni_oeste), 2).
move( (escolaPsicologia, uni_oeste), (educacao, uni_oeste), 2).
move( (educacao, uni_oeste), (complexoPedagogico2, uni_sul), 5).
move( (complexoPedagogico2, uni_sul), (escolaEconomia, uni_centro), 5).
move( (complexoPedagogico2, uni_sul), (escolaEngenharia1, uni_centro), 4).
move( (complexoPedagogico2, uni_sul), (escolaPedagogico3, uni_centro), 3).
move( (complexoPedagogico3, uni_centro), (escolaEngenharia1, uni_centro), 2).
move( (complexoPedagogico2, uni_centro), (biblioteca, uni_sul), 2).
move( (escolaEconomia, uni_centro), (acaoSocial, uni_norte), 2).
move( (cantina, uni_norte), (acaoSocial, uni_norte), 3).
move( (acaoSocial, uni_norte), (medicina, olimpo), 5).
move( (escolaEngenharia1, uni_centro), (escolaCiencias, uni_centro), 3).
move( (biblioteca, uni_sul), (institutoLetras, uni_sul), 1).
move( (institutoLetras, uni_sul), (escolaCiencias, uni_centro), 2).
move( (complexoPedagogico1, uni_este ), (complexoDesportivo, uni_este), 6).
move( (complexoPedagogico1, uni_este), (escolaCiencias, uni_centro), 3).
move( (complexoDesportivo, uni_este), (servicosTecnicos, uni_este), 2).
move( (cantina, uni_norte), (servicosTecnicos, uni_este), 5).
move( (bioSustentabilidade, uni_oeste), (cienciasSociais, uni_oeste), 2).
move( (cienciasSociais, uni_oeste), (educacao, uni_oeste), 3).
move( (cienciasSociais, uni_oeste), (complexoPedagogico3, uni_centro) , 6).


%%--------------------------------------------
% Verifica se dois Addresses estão conectados |
%%-------------------------------------------

connected( A, B, C ) :- move( A, B, C).
connected( A, B, C ) :- move( B, A, C).




%----- "DEPTH FIRST SEARCH" -----------------------------------------------------------------------------

%%------------------
% "Algoritmo DFS"   |
%%------------------

dfs(Visitados, PontoEntrega, [PontoEntrega | Visitados], 0, PontoEntrega).

dfs(Visitados, Nodo, Caminho, Custo, PontoEntrega) :-

  connected(Nodo, NextNodo, C1),
  \+member(NextNodo, Visitados),
  dfs([Nodo | Visitados], NextNodo, Caminho, C2, PontoEntrega),

  Custo is C1 + C2.


%%-----------------------------------------------------------------------------
% Ida : PontoEntrega, Caminho, Custo                                           |
% usage : ida( (rua,freguesia), Cam, Custo).                                   |
% Realiza uma procura utilizando o depth first search de um ponto pré-definido |
% (definido no corpo da regra) até um ponto final de entrega                   |
%%-----------------------------------------------------------------------------

ida(PontoEntrega, CaminhoFinal, Custo) :-

  dfs([], (escolaEngenharia1, uni_centro), Caminho, Custo, PontoEntrega),
  reverse(Caminho, CaminhoFinal).


idaAux(PontoPartida, PontoEntrega, Caminho, Custo) :-

  dfs([], PontoPartida, Cam, Custo, PontoEntrega),
  reverse(Cam, Caminho).


%%----------------------------------------------------------------------
% Volta : PontoInicial, Caminho, Custo                                  |
% usage : volta( (rua,freguesia), Cam, Custo).                          |
% Realiza uma procura utilizando o depth first search de um ponto dado  |
% até um ponto final de entrega (definido no corpo da regra)            |
%%----------------------------------------------------------------------

volta(Inicio, Caminho, Custo) :-

  dfs([], Inicio, CaminhoAux, Custo, (escolaEngenharia1, uni_centro)),
  reverse(CaminhoAux, Caminho).




%-----"DFS com lista de packages"----------------------------


%%----------------------------------------------------------------------------------------------
% Recebe uma lista de IDs de packages para devolver uma lista com os addresses desses packages  |
% list_addresses : List of PackageID, List of addresses                                         |
% usage : list_addresses(packagesID,List).                                                      |
%%----------------------------------------------------------------------------------------------

list_addresses([PackageID],[L]) :-

  package(PackageID,_,_,_,address(Rua,Freguesia),_,_),
  L = (Rua,Freguesia).


list_addresses([PackageID | Resto],ListDest) :-

  package(PackageID,_,_,_,address(Rua,Freguesia),_,_),
  L = [(Rua,Freguesia)],
  list_addresses(Resto,ListFinal),
  append(L,ListFinal,ListDest).


%%---------------------------------------------------------
% Calcula o circuito com DFS de uma lista de addresses     |
% dfsl : Inicio, Destinos, Caminho, Distancia              |
%%---------------------------------------------------------

dfsl(LastDest,[],Caminho,Km) :-
  volta(LastDest,Caminho,Km).

dfsl(LastDest,[Dest | Resto], Caminho, Kms) :-

  idaAux(LastDest,Dest,CaminhoAux,KmsAux),
  dfsl(Dest,Resto,CaminhoAux2,KmsAux2),
  tail(CaminhoAux2,Cam3),
  append(CaminhoAux,Cam3,Caminho),
  Kms is KmsAux2 + KmsAux.



%%-
% comments
%%-

melhorSolucaoDFSL([PackageID | Resto],MelhorCaminho,MelhorCusto) :-
  findall((Caminho, Custo),

          (circuitoDFSL([PackageID | Resto], Caminho, Custo)),
          List),

  menorPL(List, (MelhorCaminho,MelhorCusto)).






  %----- "BREADTH FIRST SEARCH" -----------------------------------------------------------------------------
 

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



%-----"DFS com lista de packages"----------------------------

%%------------------------------------------------------------------------------
% Calcula todos os caminhos que passam pela lista de destinos com o metodo bfs  |
% bfsl_aux : inicio, [destinos], Caminho, Km                                    |
% 
%%------------------------------------------------------------------------------

bfsl_aux(Inicio,[],Cam,Km) :-
  bfs2((escolaEngenharia1, uni_centro),[[Inicio]],Cam),
  distance(Cam,Km).


bfsl_aux(Inicio,[Dest | Resto], Cam, Km) :-
  
  \+member(Dest,Cam),

  bfs2(Dest,[[Inicio]],CamAux),
  distance(CamAux,KmAux),

  bfsl_aux(Dest,Resto,CamAux1,KmAux1),
  tail(CamAux1,Cam2),
  
  append(CamAux,Cam2,Cam),
  Km is KmAux + KmAux1.

bfsl_aux(_,[Dest | Resto], Cam, Km) :-
  
  bfsl_aux(Dest,Resto,Cam,Km).




%%---------------------------
%
%
%%---------------------------

melhorSolucaoBFSL([PackageID | Resto],MelhorCaminho,MelhorCusto) :-
findall((Caminho, Custo),

          (circuitoBFSL([PackageID | Resto], Caminho, Custo)),
          List),

  menorPL(List, (MelhorCaminho,MelhorCusto)).






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


%%---------------------
%
%%----------------------

idslAux(PontoPartida,[],Cam,Dist) :-
  
  path(PontoPartida,(escolaEngenharia1, uni_centro),Dist,CamAux),
  reverse(CamAux,Cam).


idslAux(PontoPartida,[Dest | Resto],Cam,Dist) :-
  
  path(PontoPartida,Dest,Dist1,Cam1),
  
  reverse(Cam1,Cam1Aux),

  idslAux(Dest,Resto,Cam2,Dist2),
  
  tail(Cam2,Cam2Final),

  append(Cam1Aux,Cam2Final,Cam),
  
  Dist is Dist1 + Dist2.





%%---------------------
%
%%---------------------

melhorSolucaoIDSL([PontoEntrega | R], MelhorCaminho, MelhorCusto) :-

  circuitoIDSL([PontoEntrega | R], MelhorCaminho, MelhorCusto),
  !.








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





%----- "Pesquisas Informadas" -----------------------------------------------------------------------------


%%-----------------------------------------------------------------
% CalculaEstima : ProxNodo, NodoAtual, ValorEstimado               |
% Calcula o quão vantajoso é seguir determinado caminho recebendo  |
% o Nodo que queremos atingir e o nodo onde nos encontramos e      |
% calculando o seu valor a partir de determinada heuristica.       |
% usage : calculaEstima((rua, freguesia), (rua,freguesia), Valor)  |
%%-----------------------------------------------------------------

calculaEstima((Rua1,Freguesia1) , (Rua2,Freguesia2) , ValorEstimado ):- 
  
  connected( (Rua2,Freguesia2),(Rua1,Freguesia1), X),

  findall( Rua, address(Rua,Freguesia1), List ),
  
  length( List, V ),
  
  ValorEstimado is V * X.






%----- "Greedy Search" -----------------------------------------------------------------------------



%%------------------------------------------------------------------
% Get_Gulosa : Inicio                                               |
% Devolve o resultado de uma pesquisa gulosa                        |
% desde um estado inicial, até à central de pedidos                 |
% Como o resultado da gulosa não é apresentado na primeira solução, |
% procuramos a primeira solução que contém o ponto da base a que    |
% pretendemos chegar                                                |
%%------------------------------------------------------------------

get_gulosa(Inicio, Answer, Km):-

  findall(Caminho,resolve_gulosa(Inicio, (escolaEngenharia1, uni_centro), Caminho), List),
  gulosaEnd(List, Answer, Km).



%%-------------------------------------------------------------------
% GulosaEnd : Lista de caminhos : Primeiro caminho correto           |
% esta função procura todos os caminhos obtidos com o algoritmo de   |
% pesquisa greedy, devolvendo o primeiro que apresente a completude  |
% do caminho pretendido                                              |
%%-------------------------------------------------------------------

gulosaEnd( [A/C | _], Aaux, C) :-

  getList(A/C, Aaux),

  member((escolaEngenharia1, uni_centro), Aaux), 

  !.

gulosaEnd([_| R], Answer, C) :-
  
  gulosaEnd(R, Answer, C).



%%-----------------------------------------------------------------------
% GetList : List/something, List                                         |
% Dada uma lista com algum tipo de par associado, devolve a lista no seu |
% formato original                                                       |
%%-----------------------------------------------------------------------

getList(List/_, List).



%%---------------------------------------------------------------
% Resolve_Gulosa : PontoInicial, PontoEntrega, Caminho/Custo     |
% Utiliza o algoritmo greedy search para encontrar um caminho    |
%%---------------------------------------------------------------

resolve_gulosa(Inicio, PontoEntrega, Caminho/Custo) :-
  
  agulosa( [ [Inicio]/0/0], InvCaminho/Custo/_, PontoEntrega),

  reverse(InvCaminho, Caminho).



%%----------------------------------------------------
% Agulosa : CaminhosPossiveis, Caminho, PontoEntrega  |
% Algoritmo de pesquisa "greedy"                      |
%%----------------------------------------------------

agulosa(Caminhos, Caminho, PontoEntrega) :-
  
  obtem_melhor_gulosa(Caminhos, Caminho),

  Caminho = [PontoEntrega | _]/_/_ .


agulosa(Caminhos, SolucaoCaminho, _) :-

  obtem_melhor_gulosa(Caminhos, MelhorCaminho),

  seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),

  expande_gulosa(MelhorCaminho, ExpCaminhos),

  append(OutrosCaminhos, ExpCaminhos, NovosCaminhos),

  agulosa(NovosCaminhos, SolucaoCaminho, _).


%%---------------------------------------------------------
% Agulosa Auxiliar
% Seleciona : MelhorCaminho, Caminhos, OutrosCaminhos
% ??
%%---------------------------------------------------------
seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).


%%-------------------------------------------------
% Obtem_Melhor_Gulosa : Caminhos, Caminho
% ??
%%-------------------------------------------------

obtem_melhor_gulosa([Caminho], Caminho) :- !.

obtem_melhor_gulosa([Caminho1/C1/Est1, _/_/Est2 | Caminhos], MelhorCaminho) :-

  Est1 =< Est2, !,

  obtem_melhor_gulosa([Caminho1/C1/Est1 | Caminhos], MelhorCaminho).

obtem_melhor_gulosa([_ | Caminhos], MelhorCaminho) :-

  obtem_melhor_gulosa(Caminhos, MelhorCaminho).


%%--------------------------------------------------------
% Expande_Gulosa : Caminho, CaminhosExpandidos
% ??
%%--------------------------------------------------------

expande_gulosa(Caminho, ExpCaminhos) :-

  findall(NovoCaminho, adjacenteGulosa(Caminho, NovoCaminho), ExpCaminhos).


%%------------------------------------------------------------
% Expande_Gulosa Auxiliar
% AdjacenteGulosa : Caminho, NovoCaminho
% ???
%%------------------------------------------------------------

adjacenteGulosa([Nodo | Caminho]/Custo/_, [ProxNodo, Nodo | Caminho]/NovoCusto/Est) :-

  connected(Nodo, ProxNodo, PassoCusto),

  \+member(ProxNodo, Caminho),

  NovoCusto is Custo + PassoCusto,

  calculaEstima( ProxNodo , Nodo , Est ).




%----- "A*" --------------------------------------------------------------------------------------


%%-------------------------------------------------------------------
% get_AEstrela : Inicio, Answer, Km                                  |
% Devolve o resultado de uma pesquisa aestrela desde um estado       |
% inicial, até à central de pedidos.                                 |
% Como o resultado da aestrela não é apresentado na primeira solução,|
% procuramos a primeira solução que contém o ponto da base a que     |
% pretendemos chegar                                                 |
% usage : get_AEstrela(NodoAtual, naswer,KmIda)                      |
%%-------------------------------------------------------------------

get_AEstrela(PontoEntrega, Answer, KmIda):-
  findall(Caminho,resolve_aestrela(PontoEntrega, (escolaEngenharia1, uni_centro), Caminho), List),
  aestrelaEnd(List, Answer, KmIda).






get_AEstrela2(Inicio, Intermedio, Answer, KmIda) :-
  findall(Caminho, resolve_aestrela(Intermedio, Inicio, Caminho), List),
  aestrelaEndAux(List, Answer, KmIda, Inicio, Intermedio).




%%--------------------------------------------------------------------
% aestrelaEnd : ListaCaminhos,ListaAuxiliar,Valor                     |
% Esta função procura todos os caminhos obtidos com o algoritmo de    |
% pesquisa aestrela, devolvendo o primeiro que apresente a completude |
% do caminho pretendido.                                              |
% usage : aestrelaEnd(List,VariavelAux,Valor)                         |
%%--------------------------------------------------------------------

aestrelaEnd( [A/C | _], Aaux, C) :-
  getList(A/C, Aaux),
  member((escolaEngenharia1, uni_centro), Aaux), !.

aestrelaEnd([_| R], Answer, C) :-
  aestrelaEnd(R, Answer, C).






aestrelaEndAux( [A/C | _], Aaux, C, Inicio, PontoEntrega) :-
  getList(A/C, Aaux),
  member(PontoEntrega, Aaux),
  member(Inicio, Aaux), !.

aestrelaEndAux([_|R], Answer, C, Inicio, PontoEntrega) :-
  aestrelaEndAux(R, Answer, C, Inicio, PontoEntrega).


%%-------------------------------------------------------------------
% resolve_aestrela : PontoInicial, PontoEntrega, Caminho/Custo       |
% Utiliza o algoritmo aestrela para encontrar um caminho.            |
% usage : resolve_aestrela(NodoInicial, PontoEntrega, Caminho/Custo) |
%%-------------------------------------------------------------------

resolve_aestrela(Nodo, PontoEntrega, Caminho/Custo) :-
  aestrela([[Nodo]/0/0], InvCaminho/Custo/_, PontoEntrega),
  reverse(InvCaminho, Caminho).



%%----------------------------------------------------
% aestrela : CaminhosPossiveis, Caminho, PontoEntrega |
% Algoritmo de pesquisa "A*".                         |
% usage : aestrela(List,Caminho,PontoEntrega)         |
%%----------------------------------------------------


aestrela(Caminhos, Caminho, PontoEntrega) :-
  obtem_melhor(Caminhos, Caminho),
  Caminho = [PontoEntrega|_]/_/_.

aestrela(Caminhos, SolucaoCaminho,_) :-
  obtem_melhor(Caminhos, MelhorCaminho),
  select(MelhorCaminho, Caminhos, OutrosCaminhos),
  expande_aestrela(MelhorCaminho, ExpCaminhos),
  append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
  aestrela(NovoCaminhos, SolucaoCaminho,_).
  
  
  
  
  
aEstrelaList(PontoPartida,[], Cam, Dist) :-  
  get_AEstrela2(PontoPartida, (escolaEngenharia1, uni_centro), Cam1, Dist),
  reverse(Cam1, Cam).


aEstrelaList(Inicio, [Dest | Resto], Cam, Dist) :-  
  get_AEstrela2(Inicio, Dest, Cam1, Dist1),
  reverse(Cam1, Cam1Final),
  aEstrelaList(Dest,Resto,Cam2,Dist2),
  tail(Cam2,Cam2Final),
  append(Cam1Final,Cam2Final,Cam),
  Dist is Dist1 + Dist2.



%%------------------------------------------------------
% obtem_melhor : ListaCaminhos, Caminho                 |
% Obtem o melhor caminho baseando-se no valor do custo. |
% usage : obtem_melhor(ListaCaminhos,Caminho)           |
%%------------------------------------------------------

obtem_melhor([Caminho], Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :- 
  Custo1 + Est1 =< Custo2 + Est2, !,
  obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).

obtem_melhor([_|Caminhos], MelhorCaminho) :-
  obtem_melhor(Caminhos, MelhorCaminho).



%%---------------------------------------------------
% expande_aestrela : Caminho,ListaCaminhos           |
% Cria uma lista com todos os caminhos adjacentes ao |
% ponto em que nos encontramos.                      |
% usage : expande_aestrela(Caminho,ListaCaminhos)    |
%%---------------------------------------------------

expande_aestrela(Caminho, ExpCaminhos) :-
  findall(NovoCaminho, adjacenteAEsterla(Caminho,NovoCaminho), ExpCaminhos).



%%--------------------------------------------------
% adjacenteAEsterla : Caminho,NovoCaminho           |
& Determina as caracteristicas do caminho adjacente |
% ao nodo em que nos encontramos.                   |
% usage : adjacenteAEsterla(Caminho,NovoCaminho)    |
%%--------------------------------------------------

adjacenteAEsterla([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
  connected(Nodo, ProxNodo, PassoCusto),
  \+member(ProxNodo, Caminho),
  NovoCusto is Custo + PassoCusto,
  calculaEstima( ProxNodo , Nodo , Est ).








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
