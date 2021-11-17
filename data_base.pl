%----------------------
% File written by------
% ---------------DMdSA-
%----------------------

:- set_prolog_flag(encoding, utf8).

:- dynamic road/2.


%---------------------
% bicycle
% bicycle : Weight , Average_speed, Ecological_value -> {V,F}

% motorcycle
% motorcycle : Weight , Average_speed, Ecological_value -> {V,F}

% car
% car : Weight, Average_speed, Ecological_value -> {V,F}
%---------------------


bicycle(5, 10, 0).


motorcycle(20, 35, 1).
motorcycle(12, 35, 1).

car(100, 25,2).
car(32, 25,2).





%---------------------
% Package
% package : Codigo, Nome, Peso, Volume, Valor, Morada -> {V,F}

% package(1234, iphone20, 0.705, x, 876, morada)
% 6 arguments
%---------------------


package(344053, "IPhone20", 0.988, 20, 876, "Rua Conego Manuel Faria", "Sé") 
% :- registo().
% registo(cliente, estafeta, codigo package)
package(11234, "MSIPC-2021", 3.331, 102, 2.099, "Rua wow", "Gualtar").

package(88341, "Sushi-20peças", 2.112, 122, 44.59, "Rua 3", "Sé").

% data
% tempo-limite




% package n deveria guardar #cliente e #estafeta ?









% road : Rua, Freguesia -> {V,F}
road("Rua Conego Manuel Faria", "Sé").
road("Rua 2", "Sé").
road("Rua 3", "Sé").
road("Rua wow", "Gualtar").



% estafeta
% estafeta : ID, [h | t], Nota-media (varia consoante penalizacoes)
% 
% 
% 
% 
% 
% 
% 








