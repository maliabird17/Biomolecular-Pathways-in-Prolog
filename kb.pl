%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% KNOWLEDGE BASE %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% PI2K-Akt Signaling %%%%%%%%%%%
% Molecules in the Akt pathway (Nodes)
molecule('PI3K') :- signaling(akt), chosen_akt('PI3K').
molecule('PIP3'):- signaling(akt), chosen_akt('PIP3').
molecule('PTEN'):- signaling(akt), chosen_akt('PTEN').
molecule('PDK1'):- signaling(akt), chosen_akt('PDK1').
molecule('Akt'):- signaling(akt), chosen_akt('Akt').
molecule('IKKa'):- signaling(akt), chosen_akt('IKKa').
molecule('mTOR'):- signaling(akt), chosen_akt('mTOR').
molecule('PRAS40'):- signaling(akt), chosen_akt('PRAS40').
molecule('TBC1D7'):- signaling(akt), chosen_akt('TBC1D7').

% Interactions between molecules in the Akt pathway (Edges)
edge('PI3K', 'PIP3', activate).
edge('PTEN', 'PIP3', inhibit).
edge('PIP3', 'PDK1', activate).
edge('PDK1', 'Akt', activate).
edge('IKKa', 'mTOR', activate).
edge('Akt', 'IKKa', activate).
edge('Akt', 'PRAS40', inhibit).
edge('Akt', 'TBC1D7', inhibit).


%%%%%%%%%% Insulin Signaling %%%%%%%%%%%
% Insulin Nodes
molecule('R'):- signaling(insulin), chosen_insulin('R').
molecule('Grb2'):- signaling(insulin), chosen_insulin('Grb2').
molecule('IGF'):- signaling(insulin), chosen_insulin('IGF').
molecule('Ins'):- signaling(insulin), chosen_insulin('Ins').
molecule('Grb10'):- signaling(insulin), chosen_insulin('Grb10').
molecule('IRS1'):- signaling(insulin), chosen_insulin('IRS1').
molecule('S6K'):- signaling(insulin), chosen_insulin('S6K').
molecule('PI3K'):- signaling(insulin), chosen_insulin('PI3K').
molecule('AKT2'):- signaling(insulin), chosen_insulin('AKT2').
molecule('S6K1'):- signaling(insulin), chosen_insulin('S6K1').
molecule('INS'):- signaling(insulin), chosen_insulin('INS').
molecule('IRS'):- signaling(insulin), chosen_insulin('IRS').
molecule('IGFR'):- signaling(insulin), chosen_insulin('IGFR').


% Insulin Edges
edge('R', 'Grb2', activate).
edge('IGF', 'R', activate).
edge('Ins', 'R', activate).
edge('Grb10', 'R', inhibit).
edge('R', 'IRS1', activate).
edge('S6K', 'IRS1', activate).
edge('IRS1', 'PI3K', activate).
edge('PI3K', 'AKT2', activate).
edge('S6K1', 'IRS', inhibit).
edge('INS', 'IRS', activate).
edge('IGFR', 'IRS', activate).

%%%%%%%%%%% MAPK Signaling %%%%%%%%%%%
% MAPK Nodes
molecule('RSK'):- signaling(mapk), chosen_mapk('RSK').
molecule('TBC1D7'):- signaling(mapk), chosen_mapk('TBC1D7').
molecule('ERK1/2'):- signaling(mapk), chosen_mapk('ERK1/2').
molecule('RSK'):- signaling(mapk), chosen_mapk('RSK').
molecule('MEK'):- signaling(mapk), chosen_mapk('MEK').
molecule('Raf'):- signaling(mapk), chosen_mapk('Raf').
molecule('Ras'):- signaling(mapk), chosen_mapk('Ras').
molecule('SOS'):- signaling(mapk), chosen_mapk('SOS').
molecule('Grb2'):- signaling(mapk), chosen_mapk('Grb2').

% MAPK Edges
edge('RSK', 'TBC1D7', inhibit).
edge('ERK1/2', 'RSK', activate).
edge('ERK1/2', 'TBC1D7', inhibit).
edge('MEK', 'ERK1/2', activate).
edge('Raf', 'MEK', activate).
edge('Ras', 'Raf', activate).
edge('SOS', 'Ras', activate).
edge('Grb2', 'SOS', activate).


%%%%%%%%%%% AMPK Signaling %%%%%%%%%%%
% AMPK Nodes
molecule('STRAD'):- signaling(ampk), chosen_ampk('STRAD').
molecule('AMPK'):- signaling(ampk), chosen_ampk('AMPK').
molecule('LKB1'):- signaling(ampk), chosen_ampk('LKB1').
molecule('MO25'):- signaling(ampk), chosen_ampk('MO25').
molecule('AMP'):- signaling(ampk), chosen_ampk('AMP').
molecule('TSC1/2'):- signaling(ampk), chosen_ampk('TSC1/2').
molecule('mTOR'):- signaling(ampk), chosen_ampk('mTOR').

% AMPK Edges
edge('STRAD', 'AMPK', activate).
edge('LKB1', 'AMPK', activate).
edge('MO25', 'AMPK', activate).
edge('AMP', 'AMPK', activate).
edge('AMPK', 'TSC1/2', activate).
edge('AMPK', 'mTOR', inhibit).


%%%%%%%%%%% Other Signaling %%%%%%%%%%%
% Other Molecules/Nodes
molecule('SESN2'):- signaling(other), chosen_other('SESN2').
molecule('CASTOR1'):- signaling(other), chosen_other('CASTOR1').
molecule('GATOR2'):- signaling(other), chosen_other('GATOR2').
molecule('GATOR1'):- signaling(other), chosen_other('GATOR1').
molecule('RNF152'):- signaling(other), chosen_other('RNF152').
molecule('RagA/B'):- signaling(other), chosen_other('RagA/B').
molecule('RagC/D'):- signaling(other), chosen_other('RagC/D').
molecule('Skp2'):- signaling(other), chosen_other('Skp2').
molecule('Ragulator'):- signaling(other), chosen_other('Ragulator').
molecule('SLC38A9'):- signaling(other), chosen_other('SLC38A9').
molecule('V-ATPase'):- signaling(other), chosen_other('V-ATPase').
molecule('SLC7A5'):- signaling(other), chosen_other('SLC7A5').
molecule('FLCN'):- signaling(other), chosen_other('FLCN').
molecule('FNIP'):- signaling(other), chosen_other('FNIP').
molecule('SLC3A2'):- signaling(other), chosen_other('SLC3A2').
molecule('AKT2'):- signaling(other), chosen_other('AKT2').
molecule('GSK3B'):- signaling(other), chosen_other('GSK3B').
molecule('TSC1/2'):- signaling(other), chosen_other('TSC1/2').
molecule('Dvl'):- signaling(other), chosen_other('Dvl').
molecule('Frizzled'):- signaling(other), chosen_other('Frizzled').
molecule('LRP5/6'):- signaling(other), chosen_other('LRP5/6').
molecule('Wnt'):- signaling(other), chosen_other('Wnt').
molecule('REDD1'):- signaling(other), chosen_other('REDD1').
molecule('TNFa'):- signaling(other), chosen_other('TNFa').
molecule('TNFR'):- signaling(other), chosen_other('TNFR').
molecule('IKKB'):- signaling(other), chosen_other('IKKB').
molecule('TBC1D7'):- signaling(other), chosen_other('TBC1D7').
molecule('Rheb'):- signaling(other), chosen_other('Rheb').

% Other Edges
edge('SESN2', 'GATOR2', inhibit).
edge('CASTOR1', 'GATOR2', inhibit).
edge('GATOR2', 'GATOR1', inhibit).
edge('RNF152', 'RagA/B', inhibit).
edge('RNF152', 'RagC/D', inhibit).
edge('Skp2', 'RagA/B', inhibit).
edge('Skp2', 'RagC/D', inhibit).
edge('GATOR1', 'RagA/B', inhibit).
edge('GATOR1', 'RagC/D', inhibit).
edge('Ragulator', 'RagA/B', activate).
edge('Ragulator', 'RagC/D', activate).
edge('SLC38A9', 'Ragulator', activate).
edge('V-ATPase', 'Ragulator', activate).
edge('SLC7A5', 'RagC/D', activate).
edge('SLC7A5', 'RagA/B', activate).
edge('SLC7A5', 'FLCN', activate).
edge('SLC7A5', 'FNIP', activate).
edge('SLC3A2', 'RagC/D', activate).
edge('SLC3A2', 'RagA/B', activate).
edge('SLC3A2', 'FLCN', activate).
edge('SLC3A2', 'FNIP', activate).
edge('SLC3A2', 'SLC38A9', activate).
edge('SLC3A2', 'V-ATPase', activate).
edge('SLC7A5', 'SLC38A9', activate).
edge('SLC7A5', 'V-ATPase', activate).
edge('AKT2', 'GSK3B', activate).
edge('GSK3B', 'TSC1/2', activate).
edge('GSK3B', 'TBC1D7', activate).
edge('Dvl', 'GSK3B', inhibit).
edge('Frizzled', 'Dvl', activate).
edge('LRP5/6', 'Dvl', activate).
edge('Wnt', 'LRP5/6', activate).
edge('Wnt', 'Frizzled', activate).
edge('REDD1', 'TSC1/2', activate).
edge('TNFa', 'TNFR', activate).
edge('TNFR', 'IKKB', activate).
edge('IKKB', 'TBC1D7', inhibit).
edge('IKKB', 'TSC1/2', inhibit).
edge('TSC1/2', 'Rheb', inhibit).
edge('TBC1D7', 'Rheb', inhibit).

%%%%%%%%%%% mTOR Signaling %%%%%%%%%%%
% mTOR Nodes
molecule('RagC/D'):- signaling(mTOR), chosen_mTOR('RagC/D').
molecule('mTOR'):- signaling(mTOR), chosen_mTOR('mTOR').
molecule('RagA/B'):- signaling(mTOR), chosen_mTOR('RagA/B').
molecule('PRAS40'):- signaling(mTOR), chosen_mTOR('PRAS40').
molecule('Raptor'):- signaling(mTOR), chosen_mTOR('Raptor').
molecule('PRAS40'):- signaling(mTOR), chosen_mTOR('PRAS40').
molecule('Rheb'):- signaling(mTOR), chosen_mTOR('Rheb').
molecule('AKT2'):- signaling(mTOR), chosen_mTOR('AKT2').
molecule('S6K'):- signaling(mTOR), chosen_mTOR('S6K').
molecule('S6K1'):- signaling(mTOR), chosen_mTOR('S6K1').

% mTOR Edge
edge('RagC/D', 'mTOR', activate).
edge('RagA/B', 'mTOR', activate).
edge('PRAS40', 'Raptor', inhibit).
edge('PRAS40', 'mTOR', inhibit).
edge('Raptor', 'mTOR', activate).
edge('Rheb', 'mTOR', inhibit).
edge('AKT2', 'mTOR', activate).
edge('mTOR', 'S6K', activate).
edge('mTOR', 'S6K1', activate).



interaction(activate, activate, activate).
interaction(activate, inhibit, inhibit).
interaction(inhibit, inhibit, activate).
interaction(inhibit, activate, inhibit).

% Rules for navigating the graph 
pathway(X, Y, [Regulation], [X,Y], _) :- edge(X, Y, Regulation).

pathway(X, Y, [Regulation|Reg_Path], [X|Path], Visited) :- 
    \+ member(X, Visited),
    edge(X, Z, Regulation),
    pathway(Z, Y, Reg_Path, Path, [X|Visited]).

% Determining final regulation of molecule of interest
unwind(Output, [], Output).

unwind(Input_Dir, [H|T], Output) :-
    interaction(Input_Dir, H, Result),
    unwind(Result, T, Output).


%%% Find Paths and final regulation direction
goal(Start, End, Input_Direction, Path, Direction) :- 
    pathway(Start, End, Reg_Path, Path, []),
    unwind(Input_Direction, Reg_Path, Direction).

unique_paths(Start, End, Input_Direction, L) :- 
    setof([Path, Output_Direction], goal(Start, End, Input_Direction, Path, Output_Direction), L).


%%% All combinations of lists to molecule 
pairs(List, Pairs) :-
    findall([X,Y], (append(_,[X|R],List), member(Y,R)), Pairs).

pairs_of_goal(Start, End, Input_Direction, Pairs) :- 
    unique_paths(Start, End, Input_Direction, L),
    pairs(L, Pairs).



find_loop(Short_List, Long_List, Loop) :-
    length(Short_List, N),
    length(Prefix, N),
    append(Prefix, Loop, Long_List).


%%% FEEDBACK CHECK
feedbacks([[H1,T1|_],[H2,T2|_]], [Small_H, Small_T], [negative_feedback, Loop]) :- 
    same(H1, H2), 
    T1 \== T2,
    smaller(H1, H2, T1, T2, Small_H, Small_T, Large_H, _),
    find_loop(Small_H, Large_H, Loop).

feedbacks([[H1,T1|_],[H2,T2|_]], [Small_H, Small_T], [positive_feedback, Loop]) :- 
    same(H1, H2), 
    T1 == T2,
    smaller(H1, H2, T1, T2, Small_H, Small_T, Large_H, _),
    find_loop(Small_H, Large_H, Loop).

feedbacks([[H1,_|_],[H2,_|_]], [], []) :- 
    \+ same(H1, H2).        
        

%%% Look for concise version of pathway 
smaller(H1, H2, T1, T2, H1, T1, H2, T2) :-
    length(H1, Len1),
    length(H2, Len2),
    Len1=<Len2.

smaller(H1, H2, T1, T2, H2, T2, H1, T1) :-
    length(H1, Len1),
    length(H2, Len2),
    Len1>Len2.
    

%%% CHECK FOR FEEDBACK LOOPS
same([V], [V|_]).
same([H1|R1], [H2|R2]):- 
    H1 == H2,
    same(R1, R2).

empty([]).

synthesis(Start, End, Input_Direction, Final_Paths, Feedbacks) :-
    unique_paths(Start, End, Input_Direction, Paths),
    pairs(Paths, Pairs),
    maplist(feedbacks, Pairs, Short_Paths, Feed_Loops), 
    exclude(empty, Feed_Loops, Loops),
    list_to_set(Loops, Feedbacks),
    exclude(empty, Short_Paths, New_Paths),
    list_to_set(New_Paths, Final_Paths),
    molecule(Start),
    direction(InputDirection).

% what types of molecules would you like to select 
signaling(V) :- menuask(signaling, V, [insulin, akt, mapk, ampk, mTOR, other]).

% filtered molecules for each signaling group 
chosen_akt(V) :- menuask(chosen_akt, V, ['PI3K', 'PIP3', 'PTEN', 'PDK1', 'Akt','IKKa','mTOR', 'PRAS40','TBC1D7']). 
chosen_insulin(V) :- menuask(chosen_insulin, V, ['R','Grb2','IGF', 'Ins','Grb10', 'IRS1', 'S6K', 'PI3K', 'AKT2', 'S6K1', 'INS', 'IRS', 'IGFR']).
chosen_mapk(V):- menuask(chosen_mapk, V, ['RSK', 'TBC1D7', 'ERK1/2', 'RSK', 'MEK', 'Raf', 'Ras', 'SOS', 'Grb2', 'Grb2']).
chosen_ampk(V):- menuask(chosen_ampk, V, ['STRAD', 'AMPK','LKB1','MO25','AMP','TSC1/2','mTOR']).
chosen_other(V):- menuask(chosen_other, V, ['SESN2','CASTOR1','GATOR2','GATOR1','RNF152','RagA/B','RagC/D','Skp2','Ragulator','SLC38A9','V-ATPase','SLC7A5','FLCN','FNIP','SLC3A2','AKT2','GSK3B','TSC1/2','Dvl','Frizzled','LRP5/6','Wnt','REDD1','TNFa','TNFR','IKKB','TBC1D7','Rheb']).
chosen_mTOR(V):- menuask(chosen_mTOR, V, ['RagC/D','mTOR','RagA/B','PRAS40','Raptor','PRAS40','Rheb','AKT2','S6K','S6K1']).

% How is your chosen molecule regulated? 
direction(V) :- menuask(direction, V, [activate, inhibit]).


%%% System Framework: Same code as used in the LBA 
% so collaborated with Finn, Dilnas, and Helen on this code

%% MENU ASK
menuask(A, V, Menu):-
known(yes, A, V), % succeed if true
!. % stop looking

menuask(A, V, Menu):-
known(yes, A, _), % fail if it's not a provided value
!, fail.

% If not multivalued, and already known to be something else, don't ask again for a different value.
menuask(A, V):-
\+multivalued(A),
known(yes, A, V2),
V \== V2,
!, fail.

menuask(A, V, Menu):-
read_menu_py(A, X, Menu), % get the answer
atom_string(Z, X),
check_val(Z, A, V, Menu),
asserta(known(yes, A, Z)),
Z == V.

check_val(Z, A, V, Menu) :-
member(Z, Menu), !.

check_val(Z, A, V, Menu) :-
ask_menu_again_py(Z),
menuask(A, V, Menu).