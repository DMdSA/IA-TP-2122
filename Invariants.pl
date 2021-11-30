:- op( 900,xfy,'::' ).


%----------------------------------------
% Funções de suporte 
%----------------------------------------

solucoes(X,Y,Z) :- findall(X,Y,Z).

ins(X) :- assert(X).
ins(X) :- retract(X), !, fail.

rem(X) :- retract(X).
rem(X) :- assert(X), !, fail.

evolucao(T) :- solucoes(Invariant, +T::Invariant, List),
						ins(T),
						testI(List).

involucao(T) :- solucoes(Invariant, -T::Invariant, List),
						rem(T),
						testI(List).

testI([]).
testI([H | T]) :- H, testI(T).
%----------------------------------------



%----------------------------------------
% Invariantes Estruturais e Referenciais |
%----------------------------------------

%----------------------------------------
% TRANSPORT
%----------------------------------------
%---- Tipo do facto

+transport(Name, ID) :: (

	atom(Name),
	integer(ID)
).

%---- Unicidade do facto

+transport(Name, ID) :: (

	solucoes(ID, transport(_,ID), ID_list),
	solucoes(Name, transport(Name,_), Name_list),
	length(ID_list, ID_l),
	length(Name_list, N_l),
	(ID_l = 1, N_l = 1)
).

%---- Só posso eliminar se não houver nenhum record que o utilize
% 'só há este caso?...'

-transport(_, ID) :: (

	solucoes(ID, record(_,_,_,_,ID,_), N),
	length(N,0)
).

%----------------------------------------



%----------------------------------------
% BICYCLE
%----------------------------------------
%---- Tipo do facto

+bicycle( W, AS, EV) :: (

	integer(W),
	integer(AS),
	integer(EV)
).

%---- Unicidade e validade do facto

+bicycle(W,AS,EV) :: (

	solucoes(W, bicycle(W,_,_), W_list),
	length(W_list, 1),
	validate_transp(bicycle(W,AS,EV))
).


%---- Posso apagar uma bicycle apenas quando nenhum estafeta a usar

-bicycle(W,AS,EV) :: (

	solucoes((W,AS,EV), estafeta(_,bicycle(W,AS,EV),_), N),
	length(N,0)
).
%----------------------------------------


%----------------------------------------
% MOTORCYCLE
%----------------------------------------
%---- Tipo do facto

+motorcycle( W, AS, EV) :: (

	integer(W),
	integer(AS),
	integer(EV)
).

%---- Unicidade e validade do facto

+motorcycle(W,AS,EV) :: (

	solucoes(W, motorcycle(W,_,_), W_list),
	length(W_list, 1),
	validate_transp(motorcycle(W,AS,EV))
).

%---- Posso apagar um motorcycle apenas quando nenhum estafeta a usar

-motorcycle(W,AS,EV) :: (

	solucoes((W,AS,EV), estafeta(_,motorcycle(W,AS,EV),_), N),
	length(N,0)
).
%----------------------------------------


%----------------------------------------
% CAR
%----------------------------------------
%---- Tipo do facto

+car( W, AS, EV) :: (

	integer(W),
	integer(AS),
	integer(EV)
).

%---- Unicidade e validade do facto

+car(W,AS,EV) :: (

	solucoes(W, car(W,_,_), W_list),
	length(W_list, 1),
	validate_transp(car(W,AS,EV))
).


%---- Posso apagar um car apenas quando nenhum estafeta a usar

-car(W,AS,EV) :: (

	solucoes((W,AS,EV), estafeta(_,car(W,AS,EV),_), N),
	length(N,0)
).
%----------------------------------------

%----------------------------------------
% PACKAGE
%----------------------------------------
%---- Tipo do facto

+package(ID,P,V,Value,A,D,T) :: (

	integer(ID),
	float(P),
	integer(V),
	float(Value),
	validate_address(A),
	validate_date(D),
	integer(T)
).


%---- Unicidade do facto

+package(ID,_,_,_,_,_,_) :: (

	solucoes(ID, package(ID,_,_,_,_,_,_), IDs),
	length(IDs, 1)
).

%---- Para verificar que o tempoEspera está dentro das opções.

+package(_,_,_,_,_,_,T) :: (

	T=0; T=2; T=6; T=12; T=24; T=48; T=72; T=96; T=120
	).


%---- Para apagar um package, nenhum estafeta o pode estar a entregar

-package(ID,_,_,_,_,_,_) :: (

	solucoes(Id, (estafeta(Id,_,Pkgs), member(ID, Pkgs)), List),
	length(List,0)
).

%---- Para apagar um package, nenhum record pode existir sobre ele

-package(ID,_,_,_,_,_,_) :: (

	solucoes(ID, record(ID,_,_,_,_,_), N),
	length(N,0)
	).
%---------------------------------------



%----------------------------------------
% RECORD
%----------------------------------------
%---- Tipo do facto

+record(Pid, Cid, Eid, Ddate, Tid, Drate) :: (

	integer(Pid),
	integer(Cid),
	integer(Eid),
	validate_date(Ddate),
	integer(Tid),
	integer(Drate)
).

%---- Confirmação da existência dos IDs

+record(Pid, Cid, Eid, _, Tid, _) :: (

	package(Pid,_,_,_,_,_,_),
	client(Cid,_),
	estafeta(Eid,_,L),
	member(Pid, L),						% package -> estafeta -> record, logo record depende de ambos
	transport(_,Tid)
).

%---- Verificar se a data do record é (>=) que a data da criação do package

+record(Pid, _, _, Ddate, _, _) :: (

	package(Pid,_,_,_,_,D,_),
	isAfter(D, Ddate)
).

%---- Verificar se a rate atribuída está nos limites [0,5]

+record(_,_,_,_,Drate) :: (

	Drate >= 0, Drate =< 5
).

%---- Posso apagar um record quando nenhum estafeta tiver o seu Package id na lista

-record(ID,_,_,_,_,_) :: (

	solucoes(ID, (estafeta(_,_,Pkgs), member(ID, Pkgs)), N),
	length(N, 0)
).
%----------------------------------------

%----------------------------------------
% ESTAFETA
%----------------------------------------

%---- Tipo do facto

+estafeta(ID, T, Pkgs) :: (

	integer(ID),
	validate_transp(T),
	T,
	is_list(Pkgs)
).

%---- Unicidade

+estafeta(ID, _, _) :: (

	solucoes(ID, estafeta(ID,_,_), L),
	length(L,1)
).

%---- Verificação lista encomendas

+estafeta(_,_,Pkgs) :: (

	sort(Pkgs, Ps),						%% remover duplicados
	validate_pkg_unicity(Ps),			%% verificar que existem, pelo menos
	validate_to_deliver(Ps)				%% confirmar que todos estão por entregar
).

%---- Posso remover um estafeta se não houver nenhum record sobre ele

-estafeta(ID,_,_) :: (

	solucoes(ID, record(ID,_,_,_,_,_),N),
	length(N,0)
).

%---- Posso remover um estafeta se não tiver nenhum package para/por entregar

-estafeta(_,_,Pkgs) :: (

	length(Pkgs, 0)
).
%----------------------------------------


%----------------------------------------
% CLIENT
%----------------------------------------
%---- Tipo do facto

+client(ID, Name) :: (

	integer(ID),
	atom(Name)
).

%---- Unicidade

+client(ID, _) :: (

	solucoes(ID, client(ID,_), ID_list),
	length(ID_list, 1)
).


%---- Posso apagar um client quando não houver qualquer record sobre ele

-client(ID,_) :: (

	solucoes(ID, client(ID,_), N),
	length(N,0)
).
%----------------------------------------


%----------------------------------------
% ADDRESS
%----------------------------------------
%---- Tipo do facto

+address(R, F) :: (

	atom(R),
	atom(F)
).

%---- Unicidade

+address(R,F) :: (

	solucoes((R,F), address(R,F), List),
	length(List,1)
).

%---- Posso apagar um address se não houver qualquer package sobre ele

-address(R,F) :: (

	solucoes(ID, package(ID,_,_,_,address(R,F),_,_),N),
	length(N,0)
).
%----------------------------------------