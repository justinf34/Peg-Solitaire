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
board(crossbow, [31,32,34,35,41,42,43,44,45,53], [3]).
board(longbow, [20,30,31,41,33,43,35,45,26,36,52,53,54,63], [3]).
board(not_quite_dead, [2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], [33]).


%----- Pagodas -----%
% Simple
pagoda(simple,2,0).
pagoda(simple,3,0).
pagoda(simple,4,0).
pagoda(simple,12,0).
pagoda(simple,13,1).
pagoda(simple,14,0).
pagoda(simple,20,0).
pagoda(simple,21,0).
pagoda(simple,22,0).
pagoda(simple,23,0).
pagoda(simple,24,0).
pagoda(simple,25,0).
pagoda(simple,26,0).
pagoda(simple,30,0).
pagoda(simple,31,1).
pagoda(simple,32,0).
pagoda(simple,33,1).
pagoda(simple,34,0).
pagoda(simple,35,1).
pagoda(simple,36,0).
pagoda(simple,40,0).
pagoda(simple,41,0).
pagoda(simple,42,0).
pagoda(simple,43,0).
pagoda(simple,44,0).
pagoda(simple,45,0).
pagoda(simple,46,0).
pagoda(simple,52,0).
pagoda(simple,53,1).
pagoda(simple,54,0).
pagoda(simple,62,0).
pagoda(simple,63,0).
pagoda(simple,64,0).

%Asymmetric
pagoda(asym,2,0).
pagoda(asym,3,0).
pagoda(asym,4,0).
pagoda(asym,12,0).
pagoda(asym,13,1).
pagoda(asym,14,0).
pagoda(asym,20,-1).
pagoda(asym,21,1).
pagoda(asym,22,0).
pagoda(asym,23,1).
pagoda(asym,24,0).
pagoda(asym,25,1).
pagoda(asym,26,-1).
pagoda(asym,30,0).
pagoda(asym,31,2).
pagoda(asym,32,0).
pagoda(asym,33,2).
pagoda(asym,34,0).
pagoda(asym,35,2).
pagoda(asym,36,0).
pagoda(asym,40,-1).
pagoda(asym,41,1).
pagoda(asym,42,0).
pagoda(asym,43,1).
pagoda(asym,44,0).
pagoda(asym,45,1).
pagoda(asym,46,-1).
pagoda(asym,52,0).
pagoda(asym,53,1).
pagoda(asym,54,0).
pagoda(asym,62,0).
pagoda(asym,63,0).
pagoda(asym,64,0).


wgt(_,[],0).
wgt(P, [Pos|Rest], Wgt) :-
    (pagoda(P,Pos,Wgt);
    PWgt = 0), !,
    wgt(P, Rest, WgtRest),
    Wgt is WgtRest + PWgt.

goal_wgt(crossbow, simple, 1).



% % Independence check
% independent(_, []).
% independent(Mv, [H|T]) :-
%     overlap(Mv,H), !.
% independent(Mv, [H|T]) :-
%     lexorder(Mv,H),
%     independent(Mv, T).

% overlap((S1,E1), (S2,E2)) :-
%     jump(S1, J1, E2),
%     jump(S2,J2,E2),
%     (S1)


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


peg(Board) :-
    board(Board, Start, Goal),
    solitaire_steps(Start, Moves, Goal),
    write(Moves), nl,
    write("Starting State"),nl,
    makeBoard(Start,Goal,B), wBoard(B),
    displayBoard2(Start,Moves,Goal).




    


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
