%----------------------
% File written by------
% ---------------G28---
%----------------------

%-- De uma lista de transportes, escolhe o que será o mais ecológico

moreEcologicalTransportation([A], [A]).

moreEcologicalTransportation([ A | B], Answer) :-

    moreEcologicalTransportation(B, AnswerAux),

    moreEcological(A, AnswerAux, Answer).



%%- Comparador de transportes, visando o valor ecológico

moreEcological( (A) , (B), (A)) :- 

    getEcoValue(A,Ae), getEcoValue(B, Be),

    Ae =\= Be,
    Ae < Be, !.

moreEcological( (A), (B), (B)) :-

    getEcoValue(A,Ae), getEcoValue(B, Be),

    Ae =\= Be,
    Be < Ae, !.

% por default, será o primeiro
moreEcological( (A), (_), (A)).



%%- GetAllPossibleTransports : PackageID, Kms da viagem, Lista de transportes que poderão fazer a viagem

getAllPossibleTransports(PackageID, Km, List) :-

    package(PackageID, PWeight, _, _, _, _, _),

    %%- analisar todos os transportes para o package em questão
    getAllTransportsInfo(PWeight, Km, ListaTransportes),

    %%- filtrar quais serão aptos para transportar o package
    possibleTransports(PackageID, ListaTransportes, List),

    %%- Só nos interessa a primeira opção da função anterior
    !.


%%- Para um PACKAGE e uma lista de transportes já analisados para um determinado caminho (e o seu custo), é realizada uma filtragem
%%- de quais serão capazes de levar o PACKAGE
%%- Só a primeira opção interessa, pois contém todos os casos possíveis

%%- PossibleTransports : PackageID, (Transport, RealSpeed, DurationTime), Answer

possibleTransports(PackageID, [(transport(A,TWeight,B,C,D), RealSpeed, DurationTime)], [(transport(A,TWeight,B,C,D), RealSpeed, DurationTime)]) :-

    RealSpeed > 0,

    package(PackageID, PWeight, _, _, _, _, TempoEspera),

    PWeight =< TWeight,

    DurationTime =< TempoEspera, !.


possibleTransports(PackageID, [(transport(A,TWeight,B,C,D), RealSpeed, DurationTime) | Resto], Answer) :-

    RealSpeed > 0,

    package(PackageID, PWeight, _, _, _, _, TempoEspera),

    PWeight =< TWeight,

    DurationTime =< TempoEspera,

    List1 = [(transport(A,TWeight,B,C,D), RealSpeed, DurationTime)],

    possibleTransports(PackageID, Resto, List2),

    append(List1, List2, Answer).


possibleTransports(PackageID, [(_, _, _) | Resto], Answer) :-

    possibleTransports(PackageID, Resto, Answer).



%%- Para todos os transportes disponíveis, calcula a velocidade e tempo de viagem reais

getAllTransportsInfo(Weight, Km, List) :-

    getAllTransports(Transports),

    getTransportationINFO(Transports, Weight, Km, List).



%%- getTransportationINFO : Transport, Weight, Km, (Transport, RealSpeed, DurationTime)
%%- Para um determinado transporte, associado a um peso de um package e a distancia da viagem a que terá de recorrer,
%%- calcula a velocidade real que o mesmo conseguirá atingir, assim como o tempo dessa mesma viagem

getTransportationINFO([transport(Name,A,Speed,SpeedLoss,B)], Weight, Km, List) :-

    RealSpeed is Speed - (SpeedLoss * Weight),

    Tempo is Km/RealSpeed,

    List = [(transport(Name,A,Speed,SpeedLoss,B), RealSpeed, Tempo)].


getTransportationINFO([transport(Name,A,Speed, SpeedLoss, B) | Resto], Weight, Km, List) :-

    RealSpeed is Speed - (SpeedLoss * Weight),
    Tempo is Km/RealSpeed,

    List1 = [(transport(Name,A,Speed,SpeedLoss,B), RealSpeed, Tempo)],

    getTransportationINFO(Resto, Weight, Km, List2),

    append(List1, List2, List).
