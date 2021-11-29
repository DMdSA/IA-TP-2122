:- op( 900,xfy,'::' ).



%----------------------------------------
% Funções de suporte 
%----------------------------------------

solucoes(X,Y,Z) :- findall(X,Y,Z).

insert(X) :- assert(X).
insert(X) :- retract(X), !, fail.

evolucao(T) :- solucoes(Invariant, +T::Invariant, List),
						insert(T),
						testI(List).



testI([]).
testI([H | T]) :- H, testI(T).

%----------------------------------------



%----------------------------------------
% Invariantes Estruturais e Referenciais |
%----------------------------------------

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
%----------------------------------------

+client(ID, Name) ::(
	integer(ID),
	atom(Name)
).

+client(ID, Name) :: (
	
	solucoes(ID, client(ID,_), client_list),
	length(client_list, c_l),
	(c_l, 1)
).

+adress(Rua, Fregsia) :: (
	atom(Rua),
	atom(Fregsia)
).


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
%----------------------------------------


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
%----------------------------------------


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
%----------------------------------------


%----------------------------------------
%---- Tipo do facto

+package(ID,P,V,Value,A,D) :: (

	integer(ID),
	float(P),
	integer(V),
	float(Value),
	validate_address(A),
	validate_date(D)
).


%---- Unicidade do facto

+package(ID,_,_,_,_,_) :: (

	solucoes(ID, package(ID,_,_,_,_,_), IDs),
	length(IDs, 1)
).
%---------------------------------------




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

	package(Pid,_,_,_,_,_),
	client(Cid,_),
	estafeta(Eid,_,L),
	member(Pid, L),						% package -> estafeta -> record, logo record depende de ambos
	transport(_,Tid)
).


%---- Verificar se a data do record é (>=) que a data da criação do package

+record(Pid, _, _, Ddate, _, _) :: (

	package(Pid,_,_,_,_,D),
	isAfter(D, Ddate)
).

%---- Verificar se a rate atribuída está nos limites [0,5]

+record(_,_,_,_,Drate) :: (

	Drate >= 0, Drate =< 5
).
%----------------------------------------




% estafeta : ID, Meio, [h | t], Nota-media (varia consoante penalizacoes)
% estafeta(1, bicycle(5,10,0), [11234, 88341]).

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
%----------------------------------------
