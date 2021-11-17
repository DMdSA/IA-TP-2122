%----------------------
% File written by------
% ---------------DMdSA-
%----------------------

:- set_prolog_flag(encoding, utf8).

:- dynamic road/2 .
:- discontiguous package/8 .
:- discontiguous record/3 .


/*
---------------------
 bicycle
 bicycle : Weight , Average_speed, Ecological_value -> {V,F}

 motorcycle
 motorcycle : Weight , Average_speed, Ecological_value -> {V,F}

 car
 car : Weight, Average_speed, Ecological_value -> {V,F}
---------------------
*/


bicycle(5, 10, 0).
bicycle(3,10,1).

% Aqui digo que há 2 bicicletas, mas o objetivo não é dizer quantas, mas quais...


motorcycle(20, 35, 1).
motorcycle(12, 35, 1).

car(100, 25,2).
car(32, 25,2).





/*---------------------
Package
package : Codigo, Nome, Peso, Volume, Valor, Morada -> {V,F}

Record
record : PackageID, ClientID, EstafetaID -> {V,F}
---------------------
*/


package(344053, "IPhone20", 0.988, 20, 876, "Rua Conego Manuel Faria", "Sé", date(17,11,2021)). 
record(344053, 2215, 1).

package(11234, "MSIPC-2021", 3.331, 102, 2.099, "Rua wow", "Gualtar", date(11,11,2021)).
record(11234, 12345, 2).

package(88341, "Sushi-20peças", 2.112, 122, 44.59, "Rua 3", "Sé", date(03, 10, 2021)).
record(88341, 02223, 3).



% falta -> tempo-limite



/*
---------------------
Client
client : NIF, Nome -> {V,F}
---------------------
*/

client(12345, "Diogo Araújo").
client(88321, "Joel Araújo").
client(02223, "Mateus Coelho").
client(2215, "Diogo Rebelo").






/*
---------------------
Road
road : Rua, Freguesia -> {V,F}
---------------------
*/

road("Rua Conego Manuel Faria", "Sé").
road("Rua 2", "Sé").
road("Rua 3", "Sé").
road("Rua wow", "Gualtar").



/*
---------------------
Estafeta
stafeta : ID, [h | t], Nota-media (varia consoante penalizacoes)
---------------------
*/
