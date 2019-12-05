# Surviving Peg Solitaire

1. Crossbow
2. Longbow
3. Almost dead
4. Not quite dead
5. Full


**Note**:
</br>
* 3 - 5 Needs Pagoda functions or Independence checking
* Need both for full solution
* 60\% = Basic search + displaying the solution

```prolog
peg(crossbow).
    . 
    . 
    . 
```
</br>
print the starting board, goal

display solution, start board -> move -> next move -> move -> etc...

20% Using Pagoda functions

20% Independence checking

## Pagoda Functions
Associate to each position a weight. For a particular
board state, you can associate a weight: the sum of 
the weight where a peg sits

Pagoda functions have the property that when a jump
is performed, the weight of the board never 
increases.

If the weight of hte board gets less than the weight
of the goal position, you can stop the search.

**Pagoda Function**
* linear cost
```prolog
pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple, 33,1).
pagoda(simple,35,1).
pagoda(simple,43,1).

% Calculating the weight of a board
% wgt(P,B, Wgt)    P = name of pagoda function, B = board, wgt(+,+,-)

wgt(_,[],0).
wgt(P, [Pos|Rest], Wgt) :-
    (pagoda(P,Pos, PWgt);
    PWgt = 0), !,
    wgt(P, Rest, WgtRest),
    Wgt is WgtRest + PWgt.

% goal_wgt(G, P,GoalWgt)   G = name of game, P, GoalWgt
goal_wgt(full, simple, 1).
goal_wgt(crossbow, simple, 0).

solitaire_steps(G,B,_,[]) :- % G = name of game, _ = history(starts empty), []=                                path to goal
    final_board(G,B).

solitaire_steps(G,B,Hist,[Mv|Moves]) :-
    make_jump(B, Start, Jumped, End, NewBoard),
    Mv = (Start, End),
    independence_check(Mv, Hist),
    findall((P,W), (member(P,[simple,...]),
    wgt(P, NewBoard,W)), Wgts), % the list of pagoda functions
    check_wgts(G,Wgts),
    solitaire_steps(G, NewBoard, [Mv,Hist], Moves).
% ^ Reduces combinatronics

check_wgts(G,[]).
check_wgts(G, [P,WgtP|Rest]) :-
    goal_wgt(G, P, WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(G,Rest)


```
