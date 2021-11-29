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
query_help(1) :-
		write("#> q1(IDestafeta, Meio, Nvezes)"),nl,
		write("#> ex: q1(1, car(_,_,_), X)."),nl,nl.

query_help(2) :-
		write("#> q2(Client, [(Encomenda, Estafeta)])"), nl,
		write("#> q2(Client, Estafeta, [Encomendas])"), nl,
		write("#> q2(Client, [Encomendas], [(Encomenda,Estafeta)])"),nl,
		write("#> ex: q2(\"Diogo Araújo\",X)."),nl,
		write("#> ex: q2(02223, 1, X)."),nl,
		write("#> ex: q2(2215,[344053],X)."),nl,nl.

query_help(3) :-
		write("#> q3(Estafeta, [Clients])"),nl,
		write("#> q3(Client, [Estafetas])"),nl,
		write("#> ex: q3(1,X)."),nl,
		write("#> ex: q3(12345, X)."),nl,nl.

query_help(4) :-
		write("#> q4(Date, Value)"),nl,
		write("#> q4(Month, Year, Value)"),nl,
		write("#> q4(Year, Value)"),nl,
		write("#> ex: q4(date(18,11,2021), V)."),nl,
		write("#> ex: q4(11, 2021, V)."),nl,nl.

query_help(5) :-
		write("#> q5(Road, Ndeliveries"),nl,
		write("#> ex: q5(road(\"Rua wow\",_),X)."),nl,
		write("#> ex: q5(road(\"Rua 2\",\"Sé\"),X)."),nl,nl.

query_help(6) :-
		write("#> q6(Estafeta, Value)"),nl,
		write("#> q6(Client, Estafeta, Value)"),nl,
		write("#> ex: q6(1, X)."),nl,
		write("#> ex: q6(client(12345,_),1,X)."),nl,nl.



query_help(7).
query_help(8).
query_help(9).
query_help(10).





queries() :- 
	nl,
	findall((N,T), query(N,T), List), !,
	print_query(List), nl, 
	query_help(0, X),
	write(X), nl.



print_query([]).
print_query([(N,Text) | R]) :- 
			
			write("Query["), write(N), write("] - "), write(Text)
			,nl, print_query(R).




