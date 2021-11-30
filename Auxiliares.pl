:- dynamic validate_transp/1 .
:- dynamic isAfter/2 .
:- dynamic total_price/2 .
:- dynamic max_couple/2 .
:- dynamic verify_client/1 .
:- dynamic clientID/2 .



/*
---------------------
 Validation of a transport
 validate(Transport).
 This only confirms if a specific transport mean is accepted
---------------------
*/

validate_transp(bicycle(W,10,0)) :- 
			(W > 0 , W =< 5).

validate_transp(car(W, 25, 2)) :- 
			(W > 0, W =< 100).

validate_transp(motorcycle(W,35, 1)) :- 
			(W > 0, W =< 20).



/*
---------------------
IsAfter, confirms if a date is after another one
---------------------
*/

isAfter(date(_,_,Y,_),date(_,_,Y1,_)) :-
            (Y < Y1) , !.

isAfter(date(_,M,_,_),date(_,M1,_,_)) :-
            (M < M1) , !.

isAfter(date(D,_,_,_), date(D1,_,_,_)) :-
            (D < D1) , !.

isAfter(date(_,_,_,H),date(_,_,_,H1)) :-
            (H =< H1).

/*
---------------------

---------------------
*/

total_price(package(C,P,W,V,_,_), X) :-
                
                
                record(C,_,_,_,T,_),
                X is 2.75 + P + 0.2*W + 0.15*V + 2*T.

                
/*
---------------------

---------------------
*/
max_couple([(Est,NEnc)], (Est,NEnc)).
max_couple([(Est,NEnc) | R], (Est,NEnc)) :- 
        
        max_couple(R, (_,Aux)),
        NEnc > Aux.

max_couple([(_,NEnc) | R], (A,B)) :- 
        
        max_couple(R, (A,B)),
        B >= NEnc.


/*
---------------------
Verify_Client confirms if an "ID" or "NAME" is referent to some client
---------------------
*/

verify_client(X) :- client(X,_), ! ; client(_,X).
verify_client(client(X,Y)) :- client(X,Y).


/*
---------------------
ClientID, returns the Client ID associated with an "ID" or a "NAME"
---------------------
*/

clientID(X, X) :- client(X,_), !.
clientID(X, Y) :- client(Y,X).
clientID(client(X,_), X).


/*
---------------------
Average of list
average : List, Average -> {V,F}
---------------------
*/

average( List, Average) :-
    sum_list(List, Sum),
    length( List, Length),
    Length > 0,
    Average is Sum/Length.






% "AUXILIAR RULES, ADD TO OTHER FILE + COMMENT"
remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

agrupa([], []).
agrupa([X], [(X, 1)]).
agrupa([H | T], [(H, NTimes) | Resto]) :-
                findall(H, member(H, T), L),
                length(L, NTimesAux),
                NTimes is NTimesAux + 1,
                remover(H,T,T2),
                agrupa(T2, Resto).


filter_by_date_estafetas(Date1, Date2, Package) :-
                    findall(C, (record(_,_,C,DATE,_,_),
                    dateInBetween(DATE, Date1, Date2)), Package).


dateInBetween(date(D,M,Y,H), date(D1,M1,Y1,H1), date(D2,M2,Y2,H2)) :-
        \+isAfter(date(D,M,Y,H),date(D1,M1,Y1,H1)),
          isAfter(date(D,M,Y,H1),date(D2,M2,Y2,H2)).




% "COMMENT + OTHER FILE"
filter_by_date_Transport(Date1, Date2, Transport) :-
                    findall(C, (record(_,_,_,DATE,C,_),
                    dateInBetween(DATE, Date1, Date2)), Transport).