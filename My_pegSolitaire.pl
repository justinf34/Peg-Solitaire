% Constants 
corner([0,1,5,6,
    10,11,15,16,
    50,51,55,56,
    60,61,65,66]).

peg_holes([2,3,4,
    12,13,14,
    20,21,22,23,24,25,26,
    30,31,32,33,34,35,36,
    40,41,42,43,44,45,46,
    52,53,54,
    62,63,64]).

fullBoard([0,1,2,3,4,5,6,
    10,11,12,13,14,14,16,
    20,21,22,23,24,25,26,
    30,31,32,33,34,35,36,
    40,41,42,43,44,45,46,
    50,51,52,53,54,55,56,
    60,61,62,63,64,65,66]).


% Board States
% Full board
board(full, [
        2,  3,  4,           
       12, 13, 14,
20, 21, 22, 23, 24, 25, 26,
30, 31, 32,     34, 35, 36,
40, 41, 42, 43, 44, 45, 46,
       52, 53, 54,
       62, 63, 64
         ], [33]).

% Crossbow board
board(crossbow, [31,32, 34, 35, 41, 42, 43, 44, 45, 53], [3]).

% Longbow board
board(longbow, [20, 26, 30, 31, 33, 35 ,36, 41, 43, 45, 52, 53, 54,63], [3]).

% Not quite dead board
board(notquitedead, [
        2, 3,   4,           
       12,     14,
20, 21, 22, 23, 24, 25, 26,
30,     32,         35, 36,
40, 41, 42, 43, 44, 45, 46,
       52,     54,
       62,     64
           ], [33]).

% Half dead board
board(halfdead, [
20,     22, 23, 24, 
30,             34, 35,
40, 41, 42, 43, 44, 45,
       52,     54,
       62,     64
           ], [33]).

% Not dead board
board(notdead, [
       22, 23, 24, 
               34, 35,
       42, 43, 44
                   ], [33]).


%----- Pagodas -----%
% Simple
pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,53,1).
pagoda(simple,54,0).
pagoda(simple,62,0).
pagoda(simple,63,0).
pagoda(simple,64,0).

%Asymmetric
pagoda(asym,13,1).
pagoda(asym,20,(-1)).
pagoda(asym,21,1).
pagoda(asym,23,1).
pagoda(asym,25,1).
pagoda(asym,26,(-1)).
pagoda(asym,31,2).
pagoda(asym,33,2).
pagoda(asym,35,2).
pagoda(asym,40,(-1)).
pagoda(asym,41,1).
pagoda(asym,43,1).
pagoda(asym,45,1).
pagoda(asym,46,(-1)).
pagoda(asym,53,1).


% Asymmetric Rotated 90 degrees
pagoda(asym90,2,(-1)).
pagoda(asym90,4,(-1)).
pagoda(asym90,12,1).
pagoda(asym90,13,2).
pagoda(asym90,14,1).
pagoda(asym90,31,1).
pagoda(asym90,32,1).
pagoda(asym90,33,2).
pagoda(asym90,34,1).
pagoda(asym90,35,1).
pagoda(asym90,52,1).
pagoda(asym90,53,2).
pagoda(asym90,54,1).
pagoda(asym90,62,(-1)).
pagoda(asym90,64,(-1)).

% Strong
pagoda(strong,3,21).
pagoda(strong,13,13).
pagoda(strong,20,(-8)).
pagoda(strong,21,8).
pagoda(strong,23,8).
pagoda(strong,25,8).
pagoda(strong,26,(-8)).
pagoda(strong,30,5).
pagoda(strong,31,5).
pagoda(strong,33,5).
pagoda(strong,35,5).
pagoda(strong,36,5).
pagoda(strong,40,(-3)).
pagoda(strong,41,3).
pagoda(strong,43,3).
pagoda(strong,45,3).
pagoda(strong,46,(-3)).
pagoda(strong,53,2).
pagoda(strong,63,1).



wgt(_,[],0).
wgt(P, [Pos|Rest], Wgt) :-
    (pagoda(P,Pos,PWgt);
    PWgt = 0), 
    !,
    wgt(P, Rest, WgtRest),
    Wgt is WgtRest + PWgt.



check_wgts(_,[]).
check_wgts(Game, [(Pagoda,WgtP)|Rest]) :-
    board(Game,_,Goal),
    wgt(Pagoda,Goal, WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(Game,Rest).



% Independence check
independence_check(_, []).
independence_check(Mv, [H|_]) :-
    overlap(Mv,H), !.
independence_check(Mv, [H|T]) :-
    lexorder(Mv,H),
    independence_check(Mv, T).

overlap((S1,E1), (S2,E2)) :-
    jump(S1, J1, E1),
    jump(S2,J2,E2),
    (S1 = S2; S1 = J2; S1 = E2;
    J1 = S2; J1 = J2; J1 = E2;
    E1 = S2; E1 = J2; E1 = E2;
    S2 = S1; S2 = J1; S2 = E1;
    J2 = S1; J2 = J1; J2 = E1;
    E2 = S1; E2 = J1; E2 = E1).

lexorder((S1,E1), (S2, E2)) :-
    jump(S1,J1,E1),
    jump(S2,J2,E2),
    X is S1 + J1 + E1,
    Y is S2 + J2 + E2,
    X =< Y.
    

% Boarder check
onboard(Pos) :- 2 =< Pos, Pos =< 4.
onboard(Pos) :- 12 =< Pos, Pos =< 14.
onboard(Pos) :- 20 =< Pos, Pos =< 26.
onboard(Pos) :- 30 =< Pos, Pos =< 36.
onboard(Pos) :- 40 =< Pos, Pos =< 46.
onboard(Pos) :- 52 =< Pos, Pos =< 54.
onboard(Pos) :- 62 =< Pos, Pos =< 64.


% Jumping right
jump(Start, Jumped, End) :-
    Jumped is Start + 1,
    End is Start + 2,
    onboard(Start), onboard(Jumped), onboard(End).


% Jumping left
jump(Start, Jumped, End) :-
    Jumped is Start - 1,
    End is Start - 2,
    onboard(Start), onboard(Jumped), onboard(End).

% Jumping down
jump(Start, Jumped, End) :-
    Jumped is Start + 10,
    End is Start + 20,
    onboard(Start), onboard(Jumped), onboard(End).

% Jumping up
jump(Start, Jumped, End) :-
    Jumped is Start - 10,
    End is Start - 20,
    onboard(Start), onboard(Jumped), onboard(End).


solitaire_move(SB,(Start,End), [End|SB2]) :-
    remove(Start, SB, SB1), 
    jump(Start, Jumped, End),
    remove(Jumped,SB1, SB2),
    not(member(End, SB2)).


remove(X, [X|T], T).                % Remove the first occurence of X
remove(X, [H|T], [H|R]) :-
    remove(X, T, R).                % If Head of list is not X, then move on the the tail


solitaire_steps(SB, [MV|Moves],GB) :-
    solitaire_move(SB, MV, SB1),
    solitaire_steps(SB1, Moves, GB), !.
solitaire_steps(GB, [], GB).


solitaire_stepsModed(Game,B,_,[]) :-
    board(Game,_,B).

solitaire_stepsModed(Game,B,Hist,[Mv|Moves]) :-
    solitaire_move(B, Mv, NewBoard),
    independence_check(Mv,Hist),
    findall((P,W), (member(P,[asym,asym90,strong]), wgt(P,NewBoard, W)), Wgts),
    check_wgts(Game, Wgts),
    solitaire_stepsModed(Game,NewBoard,[Mv|Hist], Moves), !.

peg(Board) :-
    board(Board, Start, _),
    solitaire_stepsModed(Board,Start, [], Moves),
    write(Moves), nl,
    write("Starting State"),nl,
    makeBoard(Start,Goal,B), wBoard(B),
    displayBoard2(Start,Moves,Goal), !.


% ---------- Show Board ----------%
displayBoard2(_,[],_):- !.
displayBoard2(GB,_,GB) :-
    makeBoard([],GB, Board),
    wBoard(Board), !.

displayBoard2(SB,[(Start,End)|Rest],GB) :-
    solitaire_move(SB, (Start,End), NextBoard),
    makeBoard(NextBoard,[GB],Board),
    wBoard(Board),
    displayBoard2(NextBoard,Rest,GB).


displayBoard(Board) :-
    board(Board, Start, [Goal|_]),
    makeBoard(Start,Goal, B),
    wBoard(B).

wBoard([A1,B1,C1,D1,E1,F1,G1,A2,B2,C2,D2,E2,F2,G2,A3,B3,C3,D3,E3,F3,G3,A4,B4,C4,D4,E4,F4,G4,A5,B5,C5,D5,E5,F5,G5,A6,B6,C6,D6,E6,F6,G6,A7,B7,C7,D7,E7,F7,G7]) :-
    write([A1,B1,C1,D1,E1,F1,G1]), nl,
    write([A2,B2,C2,D2,E2,F2,G2]), nl,
    write([A3,B3,C3,D3,E3,F3,G3]), nl,
    write([A4,B4,C4,D4,E4,F4,G4]), nl,
    write([A5,B5,C5,D5,E5,F5,G5]), nl,
    write([A6,B6,C6,D6,E6,F6,G6]), nl,
    write([A7,B7,C7,D7,E7,F7,G7]), nl, nl.
    

makeBoard(Start, Goal, Board) :-
    fullBoard(B),
    nvidiaGTX(Start, Goal, B, Board).


nvidiaGTX(_, _, [], _).
nvidiaGTX(Start, G, [Y|Ys], ["o"| Rest]) :-
    Y = G,
    peg_holes(H),
    member(Y,H),
    nvidiaGTX(Start, G, Ys, Rest).
nvidiaGTX(Start, G, [Y|Ys], ["x"| Rest]) :-
    member(Y, Start),
    peg_holes(H),
    member(Y, H),
    nvidiaGTX(Start, G, Ys, Rest).
nvidiaGTX(Start, G, [Y|Ys], ["-"|Rest]) :-
    not(member(Y,Start)),
    peg_holes(H),
    member(Y, H),
    G \= Y,
    nvidiaGTX(Start, G, Ys, Rest).
nvidiaGTX(Start, G, [Y|Ys], [" "|Rest]) :-
    peg_holes(H),
    not(member(Y,H)),
    corner(C),
    member(Y, C),
    nvidiaGTX(Start, G, Ys, Rest).
