%----------------------
% File written by------
% ---------------G28---
%----------------------


%%----------------------------------------------------------------
% MoreEcologicalTransportation : Transports, Transport            |
% De uma lista de transportes, escolhe qual será mais ecológico   |
%                                                                 |
% usage : moreEcologicalTransportation([transport(...)], Answer). |
%%----------------------------------------------------------------

moreEcologicalTransportation([A], [A]).

moreEcologicalTransportation([ A | B], Answer) :-

    moreEcologicalTransportation(B, AnswerAux),

    moreEcological(A, AnswerAux, Answer).




%%---------------------------------------------------------------------
% moreEcological : Transport, Transport, Transport                     |
% Comparador de transportes, atentando o valor ecológico               |
%                                                                      |
% usage : moreEcological((transport(...)), (transport(...)) , Answer). |
%%---------------------------------------------------------------------

moreEcological( (A) , (B), (A)) :- 

    getEcoValue(A,Ae), getEcoValue(B, Be),

    Ae =\= Be,
    Ae < Be, !.

moreEcological( (A), (B), (B)) :-

    getEcoValue(A,Ae), getEcoValue(B, Be),

    Ae =\= Be,
    Be < Ae, !.

%- por default, será o primeiro
moreEcological( (A), (_), (A)).



%%------------------------------------------------------------------------------------------------------------------
% GetAllPossibleTransports : PackageID, Kms da viagem, Lista de transportes que poderão fazer a viagem              |
%                                                                                                                   |
% Para um packageID, é realizada uma análise sobre todos os transportes disponíveis nos quais se estuda             |
% a velocidade média e a duração da viagem que realizariam caso transportassem o respetivo package, ao qual         |
% é feita uma filtragem para poder escolher apenas os transportes que conseguiriam, de facto, cumprir os requisitos |
% necessários (velocidade, limite carga, tempo).                                                                    |
%                                                                                                                   |
% usage : getAllPossibleTransports(101, Km, List).                                                                  |
%%------------------------------------------------------------------------------------------------------------------


getAllPossibleTransports(PackageID, Km, List) :-

    package(PackageID, PWeight, _, _, _, _, _),

    %%- analisar todos os transportes para o package em questão
    getAllTransportsInfo(PWeight, Km, ListaTransportes),

    %%- filtrar quais serão aptos para transportar o package
    possibleTransports(PackageID, ListaTransportes, List).


getAllPossibleTransports(PackageID, Km, A) :-
    
    package(PackageID, Weight, _, _, _, _, _),

    getTransportationINFO([transport('Carrinha', 600, 55, 0.07, 3)], Weight, Km, A).


%%--------------------------------------------------------------------------------------------------------------------------------
% PossibleTransports : PackageID, (Transport, RealSpeed, DurationTime), Answer                                                    |
% Para um PACKAGE e uma lista de transportes já analisados para um determinado caminho (e o seu custo), é realizada uma filtragem |
% de quais serão capazes de levar o PACKAGE                                                                                       |
% Só a primeira opção interessa, pois contém todos os casos possíveis                                                             |
%%--------------------------------------------------------------------------------------------------------------------------------

possibleTransports(PackageID, [(transport(A,TWeight,B,C,D), RealSpeed, DurationTime)], [(transport(A,TWeight,B,C,D), RealSpeed, DurationHours)]) :-

    RealSpeed > 0,

    package(PackageID, PWeight, _, _, _, _, TempoEspera),

    PWeight =< TWeight,

    DurationTime =< TempoEspera,

    converterHoras(DurationTime, DurationHours)
    .


possibleTransports(PackageID, [(transport(A,TWeight,B,C,D), RealSpeed, DurationTime) | Resto], Answer) :-

    RealSpeed > 0,

    package(PackageID, PWeight, _, _, _, _, TempoEspera),

    PWeight =< TWeight,

    DurationTime =< TempoEspera,

    converterHoras(DurationTime, DurationHours),

    List1 = [(transport(A,TWeight,B,C,D), RealSpeed, DurationHours)],

    possibleTransports(PackageID, Resto, List2),

    append(List1, List2, Answer).


possibleTransports(_, [], []).

possibleTransports(PackageID, [(_, _, _) | Resto], Answer) :-

    possibleTransports(PackageID, Resto, Answer).



%%----------------------------------------------------------------------------------------
% GetAllTransportsInfo : PackageWeight, Distance, List                                    |
% Para todos os transportes disponíveis, calcula a velocidade e tempo de viagem reais que |
% cumpririam, caso levassem o respetivo peso introduzido                                  |
%                                                                                         |
% usage : getAllTransportsInfo(50, 10, List).                                             |
%%----------------------------------------------------------------------------------------

getAllTransportsInfo(Weight, Km, List) :-

    getAllTransports(Transports),

    getTransportationINFO(Transports, Weight, Km, List).



%%------------------------------------------------------------------------------------------------------------------
% getTransportationINFO : Transport, Weight, Km, (Transport, RealSpeed, DurationTime)                               |
% Para um determinado transporte, associado a um peso de um package e a distancia da viagem a que terá de recorrer, |
% calcula a velocidade real que o mesmo conseguirá atingir, assim como o tempo dessa mesma viagem                   |
%%------------------------------------------------------------------------------------------------------------------

getTransportationINFO([transport(Name,A,Speed,SpeedLoss,B)], Weight, Km, List) :-

    RealSpeed is Speed - (SpeedLoss * Weight),

    (RealSpeed > 0 -> Real is RealSpeed; Real is 1),

    Tempo is Km/Real,

    List = [(transport(Name,A,Speed,SpeedLoss,B), Real, Tempo)].


getTransportationINFO([transport(Name,A,Speed, SpeedLoss, B) | Resto], Weight, Km, List) :-

    RealSpeed is Speed - (SpeedLoss * Weight),

    (RealSpeed > 0 -> Real is RealSpeed; Real is 1),

    Tempo is Km/Real,

    List1 = [(transport(Name,A,Speed,SpeedLoss,B), Real, Tempo)],

    getTransportationINFO(Resto, Weight, Km, List2),

    append(List1, List2, List).
