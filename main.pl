%----------------------
% File written by------
% ---------------DMdSA-
%----------------------

:- consult('data_base.pl').
:- set_prolog_flag(encoding, utf8).
:- discontiguous print/1.

cls :- write('\e[H\e[2J').
% make. to reload


:- dynamic validate/1.





%---------------------
% Ecological aspect
%		0-1-2 (?)
% 0 -> bicycle
% 2 -> motorcycle (less/,more polluting)
% 1 -> car (less/,more polluting)
%---------------------

% Allow lower values for a package´s weight!
% Allow different values for average speed? how to controll that?





%---------------------
% Validation of a transport
% validate(Transport).
% This only confirms if a specific transport mean is accepted
%---------------------
 
validate(bicycle(W,10,1)) :- (W > 0 , W =< 5).
validate(motorcycle(W,35, ECO)) :- (W > 0, W =< 20), (ECO >= 2 , ECO =< 3) .
validate(car(W, 25, ECO)) :- (W > 0, W =< 100), ECO >= 1, ECO =< 3 .




%---------------------
% Print a transport´s info
%---------------------

print(bicycle(W,AS,EV)) :- bicycle(W,AS,EV), 
			write("Transport: bicycle, Weight allowed: "), write(W),
			write("kg, Average Speed: "), write(AS), write("Km/h, Ecological Value: "),
			write(EV),nl,nl.
print(motorcycle(W,AS,EV)) :-
			write("Transport: motorcycle, Weight allowed: "), write(W),
			write("kg, Average Speed: "), write(AS), write("Km/h, Ecological Value: "),
			write(EV),nl,nl.
print(car(W,AS,EV)) :-
			write("Transport: motorcycle, Weight allowed: "), write(W),
			write("kg, Average Speed: "), write(AS), write("Km/h, Ecological Value: "),
			write(EV),nl,nl.




%---------------------
% Print a package´s info
%---------------------

%print(package(A,B,C,D,E,F)):- package(A,B,C,D,E,F), 
%			write("Código: "),write(A),nl, write("Produto: "),
%			write(B), write(", Peso: "), write(C), write("Kg, Volume: "), 
%			write(D), write("SI"), nl,
%			write("Preco: "), write(E),nl,
%			write("Morada: "), write(F),nl,nl.


%---------------------
% Generate a package price
%---------------------

% gen_price(package(A,B,C,D,E,F)) :- etc






%---------------------
% Check if a package exists in database
% val_package : Package -> {V,F}
%---------------------

val_package(package(A,_,_,_,_,X,Y)) :- 
		package(A,_,_,_,_,X,Y), 
		road(X,Y), 
		nl, write("Package exists"), nl, nl.





% Commands available:
% print(bicycle(5,10,1)).
% package(344053, A,B,C,D,E, F), print(package(344053, A,B,C,D,E)).
% validate(bicycle(1,10,1)).
% val_package(package(344053,_,_,_,_,E,F)).