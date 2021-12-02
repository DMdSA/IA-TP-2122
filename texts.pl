query( 1, "Identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico").
query( 2, "Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente").
query( 3, "Identificar os clientes servidos por um determinado estafeta").
query( 4, "Calcular o valor faturado pela Green Distribution num determinado dia").
query( 5, "Identificar quais as zonas (rua e/ou freguesia) com maior volume de entregas por parte da Green Distribution").
query( 6, "Calcular a classificação média de satisfação de cliente para um determinado estafeta").
query( 7, "Identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo").
query( 8, "Identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo").
query( 9, "Calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo").
query( 10, "Calcular o peso total transportado por estafeta num determinado dia").

query_help(0, "#> write \"query_help('number')\" to know more(...)").

query_help(1) :-nl,
	write("#> q1(IDestafeta, Meio, Nvezes)"),nl,
	write("#> ex: q1(E, transport(\'Car\',_,_,_), X)."),nl,
	write("#> ex: q1(E, 'Bicycle', X)."),nl,nl.

query_help(2) :-nl,
	write("#> q2(Client, [(Encomenda, Estafeta)])"), nl,
	write("#> q2(Client, Estafeta, [Encomendas])"), nl,
	write("#> q2(Client, [Encomendas], [(Encomenda,Estafeta)])"),nl,
	write("#> ex: q2(\'Diogo Araújo\',X)."),nl,
	write("#> ex: q2(10000, 1, X)."),nl,
	write("#> ex: q2(10000,[1000000],X)."),nl,nl.

query_help(3) :-nl,
	write("#> q3(Estafeta, [Clients])"),nl,
	write("#> q3(Client, [Estafetas])"),nl,
	write("#> ex: q3(1,X)."),nl,
	write("#> ex: q3(10003, X)."),nl,nl.

query_help(4) :-nl,
	write("#> q4(Date, Value)"),nl,
	write("#> q4(Month, Year, Value)"),nl,
	write("#> q4(Year, Value)"),nl,
	write("#> ex: q4(date(18,11,2021,_), V)."),nl,
	write("#> ex: q4(11, 2021, V)."),nl,nl.

query_help(5) :-nl,
	write("#> q5(Address, Ndeliveries, IDS)"),nl,
	write("#> q5(Freguesia, N, IDS)"),nl,
	write("#> ex: q5(\'Cabreiros\'),N, IDS)."),nl,
	write("#> ex: q5(address(\'Rua da Escola\',\'Crespos\'),N, IDS)."),nl,nl.

query_help(6) :-nl,
	write("#> q6(Estafeta, Value)"),nl,
	write("#> q6(Client, Estafeta, Value)"),nl,
	write("#> ex: q6(1, X)."),nl,
	write("#> ex: q6(client('Joel Araújo',_),1,X)."),nl,nl.

query_help(7) :-nl,
        write("#> q7(Date, Value)"),nl,
        write("#> q7(Date, Date, Value)"),nl,
        write("#> ex: q7(date(18,11,2021,_),X)."),nl,
        write("#> ex: q7(date(3,10,2021,12),date(17,11,2021,12),X)."),nl,nl.

query_help(8) :-nl,
        write("#> q8(Date, Value)"),nl,
        write("#> q8(Date, Date, Value)"),nl,
        write("#> ex: q8(date(3,10,2021,_),X)."),nl,
        write("#> ex: q8(date(3,10,2021,12),date(17,11,2021,12),X)."),nl,nl.

query_help(9) :-nl,
        write("#> q9(Date, Date, Total, Entregues, ÑEntregues)"),nl,
        write("#> ex: q9(date(3,10,2021,12),date(17,11,2021,16),T,E,N)."),nl.

query_help(10) :-nl,
        write("#> q10(Estafeta, Data, Weight)"),nl,
        write("#> q10(Date, Answer)"),nl,
        write("#> ex: q10,(1, date(18,11,2021,12), P)."),nl,
        write("#> ex: q10(date(18,11,2021,12), X)."), nl,nl.

queries() :-
	nl,
	findall((N,T), query(N,T), List), !,
	print_query(List), nl, 
	query_help(0, X),
	write(X), nl.

print_query([]).
print_query([(N,Text) | R]) :- nl,
			
			write("Query["), write(N), write("] - "), write(Text)
			,nl, print_query(R).