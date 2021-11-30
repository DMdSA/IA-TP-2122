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

isAfter(date(_,_,Y),date(_,_,Y1)) :-
            (Y < Y1) , !.

isAfter(date(_,M,Y),date(_,M1,Y)) :-
            (M < M1) , !.

isAfter(date(D,M,Y), date(D1,M,Y)) :-
            (D =< D1).


/*
---------------------

---------------------
*/
total_price(package(C,P,W,V,_,_,_), X) :-
                
                
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