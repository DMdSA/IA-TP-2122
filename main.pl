%----------------------
% File written by------
% ---------------DMdSA-
%----------------------
cls :- write('\e[H\e[2J').
% make. to reload




:- set_prolog_flag(encoding, utf8).
:- consult('data_base.pl').
:- discontiguous print/1.
:- discontiguous package/8 .
:- discontiguous record/3 .





/*
---------------------
 Ecological aspect
		0-1-2
 0 -> bicycle
 1 -> car
 2 -> motorcycle
---------------------
*/



/*
---------------------
 Validation of a transport
 validate(Transport).
 This only confirms if a specific transport mean is accepted
---------------------
*/

validate(bicycle(W,10,0)) :- 
			(W > 0 , W =< 5).

validate(car(W, 25, 1)) :- 
			(W > 0, W =< 100).

validate(motorcycle(W,35, 2)) :- 
			(W > 0, W =< 20).











%---------------------
% Generate a package price
%---------------------







%---------------------
% Check if a package exists in database
% val_package : Package -> {V,F}
%---------------------
