%----------------------
% File written by------
% ---------------G28---
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload

% :- style_check(-singleton).
:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').


g( grafo(

  [
      % cada nodo é um ADDRESS, address : Rua, Freguesia

  address( complexoPedagogico1, uni_este ),     % 1
  address( complexoPedagogico2, uni_sul ),      % 2
  address( complexoPedagogico3, uni_centro),    % 3
  address( biblioteca, uni_sul),                % 4
  address( institutoLetras, uni_sul),           % 5
  address( escolaCiencias, uni_centro),         % 6
  address( escolaEngenharia1, uni_centro),      % 7
  address( escolaEconomia, uni_centro),         % 8
  address( complexoDesportivo, uni_este),       % 9
  address( servicosTecnicos, uni_este),         % 10
  address( cantina, uni_norte),                 % 11
  address( acaoSocial, uni_norte),              % 12
  address( educacao, uni_oeste),                % 13
  address( psicologia, uni_oeste),              % 14
  address( cienciasSociais, uni_oeste),         % 15
  address( escolaEngenharia2, uni_oeste),       % 16
  address( escolaDireito, uni_oeste),           % 17
  address( bioSustentabilidade, uni_oeste),     % 18
  address( medicina, olimpo)                    % 19
  
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
    aresta(  (complexoPedagogico1, uni_este), (biblioteca, uni_sul), 1),
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


%- Starting adress, always
starting_point(address(escolaEngenharia1, uni_centro)).



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
move(  (complexoPedagogico1, uni_este), (biblioteca, uni_sul), 1).
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


start_address(address(escolaEngenharia1, uni_centro)).


move(F, S) :- move(F, S, _).
move(F, S) :- move(S, F, _).

goal((servicosTecnicos, uni_este)).


%----- DEPTH FIRST SEARCH



dfs2((Rua,Freguesia), [(Rua, Freguesia) | _], _, []).

dfs2((Rua,Freguesia), [(Rua,Freguesia) | Caminho], Custo, [Last]) :-
      
      dfs_alg2((Rua,Freguesia), Caminho, Custo, Last).




dfs2((Rua, Freguesia), [(Rua,Freguesia) | CaminhoFinal], CustoFinal, [F , S | R]) :-

    %% procura o dfs para o F
    dfs_alg2((Rua,Freguesia), Caminho, Custo, F),

    %% se 'S' já for membro, não vale a pena voltar a procurar caminho por ele
    member(S, Caminho),

    write('wat'),

    %% começar uma nova procura a partir do último
    last(Caminho, Last),

    %% nova procura até ao prox elemento
    dfs_alg2(Last, [Last | CaminhoAux], CustoAux, R),

    %% juntar ambos os caminhos
    append(Caminho, CaminhoAux, CaminhoFinal),

    %% juntar ambos os custos
    CustoFinal is Custo + CustoAux.



dfs2((Rua,Freguesia), [(Rua,Freguesia) | CaminhoFinal], CustoFinal, [F, S | R]) :-
    
    dfs_alg2((Rua,Freguesia), Caminho, Custo, F),

    \+member(S,Caminho),

    write('oi'),

    last(Caminho, Last),

    dfs_alg2(Last, [Last | CaminhoAux], CustoAux, [S|R]),

    append(Caminho, CaminhoAux, CaminhoFinal),

    CustoFinal is Custo + CustoAux.










dfs_alg2(Last, [], 0, Last).

dfs_alg2(Inicio, [NextNodo | Resto], Custo, Last) :-

    move(Inicio, NextNodo, CustoAux),

    dfs_alg2(NextNodo, Resto, CustoAux2, Last),

    Custo is CustoAux2 + CustoAux.













dfs((Rua, Freguesia), [(Rua, Freguesia) | Caminho], Custo)  :-
        
        dfs_alg((Rua, Freguesia), Caminho, Custo).


%% Chegamos ao fim do circuito quando chegarmos ao address goal

dfs_alg(Address, [], 0) :-
    
    goal(Address).

dfs_alg(Inicio, [NextNodo | Resto], Custo) :-
    
    move(Inicio, NextNodo, CustoAux),

    dfs_alg(NextNodo, Resto, CustoAux2),

    Custo is CustoAux2 + CustoAux.


dfs_findall(List) :- findall((Caminho,Custo), (dfs((escolaEngenharia1, uni_centro), Caminho, Custo)), List).


