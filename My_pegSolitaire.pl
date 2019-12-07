% ---------- Constants ---------- %
% Array representation of the "no peg" corners
corner([0,1,5,6,
    10,11,15,16,
    50,51,55,56,
    60,61,65,66]).

% Array representation of the peg holes
peg_holes([2,3,4,
    12,13,14,
    20,21,22,23,24,25,26,
    30,31,32,33,34,35,36,
    40,41,42,43,44,45,46,
    52,53,54,
    62,63,64]).

% Array representation of the full board
fullBoard([0,1,2,3,4,5,6,
    10,11,12,13,14,14,16,
    20,21,22,23,24,25,26,
    30,31,32,33,34,35,36,
    40,41,42,43,44,45,46,
    50,51,52,53,54,55,56,
    60,61,62,63,64,65,66]).


% ---------- Board States ---------- %
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


% ---------- Pagodas ---------- %
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


pagoda(anotha,2,(-1)).
pagoda(anotha,4,(-1)).
pagoda(anotha,12,1).
pagoda(anotha,14,1).
pagoda(anotha,31,1).
pagoda(anotha,32,1).
pagoda(anotha,34,1).
pagoda(anotha,36,1).
pagoda(anotha,52,1).
pagoda(anotha,54,1).
pagoda(anotha,62,(-1)).
pagoda(anotha,64,(-1)).


pagoda(anotha2,2,(-1)).
pagoda(anotha2,4,(-1)).
pagoda(anotha2,12,1).
pagoda(anotha2,14,1).
pagoda(anotha2,30,1).
pagoda(anotha2,32,1).
pagoda(anotha2,34,1).
pagoda(anotha2,35,1).
pagoda(anotha2,52,1).
pagoda(anotha2,54,1).
pagoda(anotha2,62,(-1)).
pagoda(anotha2,64,(-1)).


% Weights of the goal for each pagodas for each game states
goal_wgt(crossbow, simple, 0).
goal_wgt(crossbow, asym, 0).
goal_wgt(crossbow, asym90, 0).
goal_wgt(crossbow, strong, 21).
goal_wgt(crossbow, anotha, 0).
goal_wgt(crossbow, anotha2, 0).

goal_wgt(longbow, simple, 0).
goal_wgt(longbow, asym, 0).
goal_wgt(longbow, asym90, 0).
goal_wgt(longbow, strong, 21).
goal_wgt(longbow, anotha, 0).
goal_wgt(longbow, anotha2, 0).



goal_wgt(notquitedead, simple, 1).
goal_wgt(notquitedead, asym, 2).
goal_wgt(notquitedead, asym90, 2).
goal_wgt(notquitedead, strong, 5).
goal_wgt(notquitedead, anotha, 0).
goal_wgt(notquitedead, anotha2, 0).


goal_wgt(halfdead, simple, 1).
goal_wgt(halfdead, asym, 2).
goal_wgt(halfdead, asym90, 2).
goal_wgt(halfdead, strong, 5).
goal_wgt(halfdead, anotha, 0).
goal_wgt(halfdead, anotha2, 0).

goal_wgt(notdead, simple, 1).
goal_wgt(notdead, asym, 2).
goal_wgt(notdead, asym90, 2).
goal_wgt(notdead, strong, 5).
goal_wgt(notdead, anotha, 0).
goal_wgt(notdead, anotha2, 0).

goal_wgt(full, simple, 1).
goal_wgt(full, asym, 2).
goal_wgt(full, asym90, 2).
goal_wgt(full, strong, 5).
goal_wgt(full, anotha, 0).
goal_wgt(full, anotha2, 0).


% Determines the weight of the board using pagoda
wgt(_,[],0).
wgt(P, [Pos|Rest], Wgt) :-
    (pagoda(P,Pos,PWgt);
    PWgt = 0), 
    !,
    wgt(P, Rest, WgtRest),
    Wgt is WgtRest + PWgt.          % Sums the total pagoda weights

check_wgts(_,[]).
check_wgts(Game, [(Pagoda,WgtP)|Rest]) :-
    goal_wgt(Game, Pagoda, WgtGoal),
    WgtP >= WgtGoal,                   % Making sure that the weight is less greater than the weight goal
    check_wgts(Game,Rest).



% ----------Independence check ---------- %
independence_check(_, []).              % Case when there is no more history to check
independence_check(Mv, [H|_]) :-
    overlap(Mv,H), !.                   % Checks when a component of a move overlaps with another move later on
independence_check(Mv, [H|T]) :-
    lexorder(Mv,H),                     % Defines the order of which move to choose from based on a order
    independence_check(Mv, T).

% Determines if there is an overlap between 2 move 
overlap((S1,E1), (S2,E2)) :-
    jump(S1, J1, E1),           % jump is calculated to get all necessary parts of a move (Start, Jump, End)
    jump(S2,J2,E2),             
    (S1 = S2; S1 = J2; S1 = E2; % comparing components with other move
    J1 = S2; J1 = J2; J1 = E2;
    E1 = S2; E1 = J2; E1 = E2).

% Sets the order of which move to choose first
% lexorder((S1,E1), (S2, E2)) :-
%     jump(S1,J1,E1),
%     jump(S2,J2,E2),
%     X is S1 + J1 + E1,
%     Y is S2 + J2 + E2,
%     X =< Y.                 % Chooses the move that will are higher in the board  

lexorder((S1,E1), (S2, E2)) :-
    X is S1 + E1,
    Y is S2 + E2,
    X =< Y.                 % Chooses the move that will are higher in the board      
    


% ---------- Main ---------- %
% Boarder check, setting boundaries for the board and
onboard(Pos) :- 2 =< Pos, Pos =< 4.
onboard(Pos) :- 12 =< Pos, Pos =< 14.
onboard(Pos) :- 20 =< Pos, Pos =< 26.
onboard(Pos) :- 30 =< Pos, Pos =< 36.
onboard(Pos) :- 40 =< Pos, Pos =< 46.
onboard(Pos) :- 52 =< Pos, Pos =< 54.
onboard(Pos) :- 62 =< Pos, Pos =< 64.


% The jump functions determine the pieces that is eliminated
% and the end position of the board by adding or subtracting
% to the start position.

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

% apply the move to the board
solitaire_move(SB,(Start,End), [End|SB2]) :-
    remove(Start, SB, SB1),         % remove start piece from its position
    jump(Start, Jumped, End),       % determine the eaten piece
    remove(Jumped,SB1, SB2),        % remove the eaten piece
    not(member(End, SB2)).          % move the start piece to the end hole


remove(X, [X|T], T).                % Remove the first occurence of X
remove(X, [H|T], [H|R]) :-
    remove(X, T, R).                % If Head of list is not X, then move on the the tail


% calculate all the moves from start state to goal
% without using pagodas or independence checking
solitaire_steps(SB, [MV|Moves],GB) :-
    solitaire_move(SB, MV, SB1),            % Apply a move and get the new stat
    solitaire_steps(SB1, Moves, GB), !.     % Thne apply the new moves to the new state and stop when a solution is found
solitaire_steps(GB, [], GB).                % The case when the goal is reached


% This solitaire steps uses pagoda functions and independence check
% to calculate solution faster by reducing combinatronics
solitaire_stepsModed(Game,B,_,[]) :-        % The case when the goal is found
    board(Game,_,B).
solitaire_stepsModed(Game,B,Hist,[Mv|Moves]) :-
    solitaire_move(B, Mv, NewBoard),                % apply the moves
    independence_check(Mv,Hist),                    % check if this move is independent from other moves
    findall((P,W), (member(P,[asym,asym90,anotha,anotha2,strong]), wgt(P,NewBoard, W)), Wgts),  % find all pagoda weights
    check_wgts(Game, Wgts),                         % check the weight of the board
    solitaire_stepsModed(Game,NewBoard,[Mv|Hist], Moves), !.        % once a solution is found then stop, genrate new steps from new state
 
%  Main function of the game                                        % and add the move to history
peg(Board) :-
    board(Board, Start, _),                                         % Get start state and goal
    solitaire_stepsModed(Board,Start, [], Moves),                   % Generate moves to goal
    write(Moves), nl,                                               % Show steps to goal                                        
    write("Starting State"),nl,
    makeBoard(Start,Goal,B), wBoard(B),                             % Draw the initial board          
    displayBoard2(Start,Moves,Goal), !.                             % Display the other sequence of steps to goal


% ---------- Show Board ---------- %
displayBoard2(_,[],_):- !.                                          % Case when there is no moves left
displayBoard2(GB,_,GB) :-                                           % Case when the goal is found
    makeBoard([],[GB], Board),                                      % then only draw the goal point
    wBoard(Board), !.                                               % write the board

displayBoard2(SB,[(Start,End)|Rest],GB) :-                          % Case when its not the goal
    solitaire_move(SB, (Start,End), NextBoard),                     % Apply the move
    makeBoard(NextBoard,[GB],Board),                                % draw  the board
    wBoard(Board),                                                  % display the board
    displayBoard2(NextBoard,Rest,[GB]).


displayBoard(Board) :-                                              % Just for testing the display
    board(Board, Start, Goal),
    makeBoard(Start,Goal, B),
    wBoard(B).

wBoard([A1,B1,C1,D1,E1,F1,G1,A2,B2,C2,D2,E2,F2,G2,A3,B3,C3,D3,E3,F3,G3,A4,B4,C4,D4,E4,F4,G4,A5,B5,C5,D5,E5,F5,G5,A6,B6,C6,D6,E6,F6,G6,A7,B7,C7,D7,E7,F7,G7]) :-
    write([A1,B1,C1,D1,E1,F1,G1]), nl,                              % Row 1
    write([A2,B2,C2,D2,E2,F2,G2]), nl,                              % Row 2
    write([A3,B3,C3,D3,E3,F3,G3]), nl,                              % Row 3
    write([A4,B4,C4,D4,E4,F4,G4]), nl,                              % Row 4
    write([A5,B5,C5,D5,E5,F5,G5]), nl,                              % Row 5
    write([A6,B6,C6,D6,E6,F6,G6]), nl,                              % Row 6
    write([A7,B7,C7,D7,E7,F7,G7]), nl, nl.                          % Row 7
    

makeBoard(Start, [Goal], Board) :-
    fullBoard(B),                                                   % Get the original full board layout 
    nvidiaGTX(Start, [Goal], B, Board).                             % Draw the board using the current state of the game


nvidiaGTX(_, _, [], _).                                             % Base case
nvidiaGTX(Start, G, [Y|Ys], ["o"| Rest]) :-                         % To draw the goal
    Y = G,
    peg_holes(H),
    member(Y,H),
    nvidiaGTX(Start, G, Ys, Rest).
nvidiaGTX(Start, G, [Y|Ys], ["x"| Rest]) :-                         % To draw the pegs
    member(Y, Start),
    peg_holes(H),
    member(Y, H),
    nvidiaGTX(Start, G, Ys, Rest).
nvidiaGTX(Start, G, [Y|Ys], ["-"|Rest]) :-                          % Draw the empty peg holes
    not(member(Y,Start)),
    peg_holes(H),
    member(Y, H),
    G \= Y,
    nvidiaGTX(Start, G, Ys, Rest).
nvidiaGTX(Start, G, [Y|Ys], [" "|Rest]) :-                          % Corner spaces
    peg_holes(H),
    not(member(Y,H)),
    corner(C),
    member(Y, C),
    nvidiaGTX(Start, G, Ys, Rest).
