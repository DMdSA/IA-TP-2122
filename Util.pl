%----------------------
% File written by------
% ---------------G28---
%----------------------


/*
 %-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%----------
| 										  |
| 2 maneiras de medir o tempo de execução:|
| 										  |
| time( metodo(arg1, arg2) ).		  	  |
| 										  |
| call_time( metodo(arg1,arg2) , Dict).	  |
| 										  |
 %-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%----------
*/



%-----------------------------
% Devolve a tail de uma lista |
%-----------------------------

tail([], []).
tail([_ | R], R).



%----------------------------------------
% MenorInteiros : Número, Número, Número |
%----------------------------------------

menorI(A,B, A) :- A =\= B, A < B.
menorI(A,B, B) :- A =\= B, B < A.
menorI(A,A,A).


%----------------------------------
% MenorInteirosList : List, Número |
%----------------------------------

menorIL([A], A).

menorIL([A | R], Answer) :-
  
  %% extender a lista toda e comparar um a um
  menorIL(R, Aux),
  menorI(A, Aux, Answer).


%-------------------------------------------------------------
% MenorPares : (Algo, Número), (Algo, Número), (Algo, Número) |
%-------------------------------------------------------------

menorP((Cam1,C1), (_, C2), (Cam1,C1)) :- C1 =\= C2, C1 =< C2.

menorP((_,C1), (Cam2,C2), (Cam2,C2)) :- C1 =\= C2, C2 < C1.

%-- "por default, vai escoler sempre o primeiro"
menorP((Cam,C), (_,C), (Cam,C)).


%-------------------------------------------------------------
% MaiorPares : (Algo, Número), (Algo, Número), (Algo, Número) |
%-------------------------------------------------------------

maiorP((Cam1,C1), (_, C2), (Cam1,C1)) :- C1 =\= C2, C1 > C2.

maiorP((_,C1), (Cam2,C2), (Cam2,C2)) :- C1 =\= C2, C2 > C1.

%-- "por default, vai escoler sempre o primeiro"
maiorP((Cam,C), (_,C), (Cam,C)).



%--------------------------------------------
% MenorParesList : PairsList, (Algo, Número) |
%--------------------------------------------

menorPL([ (Cam, Custo) ], (Cam, Custo)).

menorPL([(Cam,C) | R], Resposta) :-

  %% extender a lista toda e comparar um a um
  menorPL(R, (Cam2, C2)),
  menorP((Cam, C), (Cam2,C2), Resposta).



%--------------------------------------------
% MaiorParesList : PairsList, (Algo, Número) |
%--------------------------------------------

maiorPL([ (Cam, Custo) ], (Cam, Custo)).

maiorPL([(Cam,C) | R], Resposta) :-

  %% extender a lista toda e comparar um a um
  maiorPL(R, (Cam2, C2)),
  maiorP((Cam, C), (Cam2,C2), Resposta).



%---------------------------------------------------------------------------------
% MenorTriplos : (Algo, Algo, Número), (Algo, Algo, Número), (Algo, Algo, Número) |
%---------------------------------------------------------------------------------

%%- "Versão em que o valor de comparação é o último do triplo"

menorT((Cam1,I1, C1), (_,_, C2), (Cam1,I1,C1)) :- C1 =\= C2, C1 =< C2.

menorT((_,_,C1), (Cam2,I2,C2), (Cam2,I2,C2)) :- C1 =\= C2, C2 < C1.

%-- "por default, vai escoler sempre o primeiro"
menorT((Cam,I1,C), (_,_,C), (Cam,I1,C)).


%%- "Versão em que o valor de comparação é o último do triplo"
menorT2((C1, A, B), (C2, _, _), (C1,A,B)) :- C1 =\= C2, C1 =< C2.

menorT2((C1, _, _), (C2, A , B), (C2, A, B)) :- C1 =\= C2, C2 < C1.

%-- "por default, vai escoler sempre o primeiro"
menorT2((C, A, B), (C, _, _), (C, A, B)).






%------------------------------------------------------
% MenorTriplosList : TriplosList, (Algo, Algo, Número) |
%------------------------------------------------------

%%- "Versão em que o valor de comparação é o último do triplo"

menorTL([ (Cam, Ida, Custo) ], (Cam, Ida, Custo)).

menorTL([(Cam,I,C) | R], Resposta) :-

  %% extender a lista toda e comparar um a um
  menorTL(R, (Cam2,I2, C2)),
  menorT((Cam, I, C), (Cam2,I2,C2), Resposta).

%%- "Versão em que o valor de comparação é o último do triplo"

menorTL2([ (C, O1, O2) ], (C, O1, O2)).

menorTL2([(C,O1,O2) | R], Resposta) :-

  %% extender a lista toda e comparar um a um
  menorTL2(R, (C2, O3, O4)),
  
  menorT2((C, O1, O2), (C2,O3,O4), Resposta).



%%---------------------------------------
% Date 								                   |
% date : Day, Month, Year, Hour -> {V,F} |
%%---------------------------------------

date(D,M,A,H) :- D >= 1, D =< 30, member(M, [4,6,9,11]),
                A > 0, H>=0, H<24, !.
date(D,M,A,H) :- D >= 1, D =< 31, member(M, [1,3,5,7,8,10,12]),
                A > 0, H>=0, H<24, !.
date(D,2,A,H) :- D >= 1, D =< 29,
                H>=0, H<24,
                A mod 4 =:= 0, A mod 100 =:= 0, A mod 400 =:= 0.
date(D,2,A,H) :- D >= 1, D =< 29,
                H>=0, H<24,
                A mod 4 =:= 0, A mod 100 =\= 0 .
date(D,2,A,H) :- D >= 1, D =< 28,
                H>=0, H<24,
                A mod 4 =:= 0, A mod 100 =:= 0, A mod 400 =\= 0.
date(D,2,A,H) :- D >= 1, D =< 28,
                H>=0, H<24,
                A mod 4 =\= 0.



%-------------------------------
% Date Auxiliar					        |
% Verifica se uma data é válida |
%-------------------------------

validate_date(date(D,M,Y,H)) :- 
                date(D,M,Y,H).




% - n.o.t. u.s.e.d.
%%----------------------------------------------------------------------
% Dado um número, articula-o na parte das unidades e das casas decimais |
% usage : getDecimalPart(1.234, U, D).                                  |
%%----------------------------------------------------------------------

getDecimalPart(Number, Unity, Decimal) :-

    Unity is floor(Number),

    Decimal is Number - Unity.


% - n.o.t. u.s.e.d.
%%---------------------------------
% Hour : Hours, Minutes, Seconds   |
%%---------------------------------

hour(A,B,C) :-

    A >= 0, A =< 23,
    B >= 0, B =< 59,
    C >= 0, C =< 59.




% - n.o.t. u.s.e.d.
%%------------------------------------------------------------------
% Converte um número na sua representação em Horas:Minutos:Segundos |
% (Não respeita os limites do horário de um dia)                    |
% usage : converterHoras(1.234, Answer).                            |
%%------------------------------------------------------------------

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