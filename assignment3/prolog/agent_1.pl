% COMP9814 Assignment3
% Option 2: Prolog (BDI Agent)
% Group14: Yisong Jiang z5123920
%		   Yangyi Huang z5015832


trigger([], goals([],[])).

trigger([truffle(X,Y,S)|Restpercepts],goals(RGoals_rest,Goals_truff)):-
	trigger(Restpercepts,goals(RGoals_rest,RGoals_truff)),
	Goals_truff = [goal(X,Y,S)|RGoals_truff].
trigger([restaurant(X,Y,S)|Restpercepts],goals(Goals_rest,RGoals_truff)):-
	trigger(Restpercepts,goals(RGoals_rest,RGoals_truff)),
	Goals_rest = [goal(X,Y,S)|RGoals_rest].

% incorporate_goals(Goals, Beliefs, Intentions, Intentions1)
rebuild([],Int,_,_,Int).
rebuild([goal(X,Y,Z)|Tail],Int,PX,PY,Int1):-
	contains(goal(X,Y,Z),Int),
	rebuild(Tail,Int,PX,PY,Int1).
rebuild([goal(X,Y,Z)|Tail],Int,PX,PY,Int1):-
	\+contains(goal(X,Y,Z),Int),
	rebuild(Tail,Int,PX,PY,Int2),
	insertgoal(goal(X,Y,Z),Int2,PX,PY,Int1).

insertgoal(goal(X,Y,Z),[],_,_,[[goal(X,Y,Z),[]]]).
insertgoal(goal(X,Y,Z),Int,_,_,Int1):-
	UnitGoal = [goal(X,Y,Z),[]],
	Int = [Head|_],
	Head = [goal(_,_,Z1),_],
	Z>Z1,
	Int1 = [UnitGoal|Int].

insertgoal(goal(X,Y,Z),Int,PX,PY,Int1):-
	Int = [Head|Tail],
	Head = [goal(_,_,Z1),_],
	Z<Z1,
	insertgoal(goal(X,Y,Z),Tail,PX,PY,Int2),
	Int1 = [Head|Int2].

insertgoal(goal(X,Y,Z),Int,PX,PY,Int1):-
	UnitGoal = [goal(X,Y,Z),[]],
	Int = [Head|_],
	Head = [goal(X1,Y1,Z1),_],
	Z=:=Z1,
	distance((X,Y),(PX,PY),NewD),
	distance((X1,Y1),(PX,PY),HeadD),
	NewD < HeadD,
	Int1 = [UnitGoal|Int].

insertgoal(goal(X,Y,Z),Int,PX,PY,Int1):-
	Int = [Head|Tail],
	Head = [goal(X1,Y1,Z1),_],
	Z=:=Z1,
	distance((X,Y),(PX,PY),NewD),
	distance((X1,Y1),(PX,PY),HeadD),
	NewD >= HeadD,
	insertgoal(goal(X,Y,Z),Tail,PX,PY,Int2),
	Int1 = [Head|Int2].


contains(goal(X,Y,_),[Head|_]):-
	Head = [goal(X,Y,_),_].
contains(goal(X,Y,_),[Head|Tail]):-
	Head = [goal(X1,Y1,_),_],
	X1 \= X,
	Y1 \= Y,
	contains(goal(X,Y,_),Tail).
contains(goal(X,Y,_),[Head|Tail]):-
	Head = [goal(X1,Y1,_),_],
	X1 =:= X,
	Y1 \= Y,
	contains(goal(X,Y,_),Tail).
contains(goal(X,Y,_),[Head|Tail]):-
	Head = [goal(X1,Y1,_),_],
	X1 \= X,
	Y1 =:= Y,
	contains(goal(X,Y,_),Tail).

allcontains([],_).
allcontains([Head|Tail],Checklist):-
	contains(Head,Checklist),
	allcontains(Tail,Checklist).

incorporate_goals(goals(Goals_rest,Goals_truff),_, intents(Int_sell,Int_pick), Intentions1):-
	allcontains(Goals_rest,Int_sell),
	allcontains(Goals_truff,Int_pick),
	Intentions1 = intents(Int_sell,Int_pick).

incorporate_goals(goals(Goals_rest,Goals_truff),beliefs(at(PX,PY),stock(_)), intents(Int_sell,Int_pick), Intentions1):-
	\+allcontains(Goals_rest,Int_sell),
	allcontains(Goals_truff,Int_pick),
	rebuild(Goals_rest,Int_sell,PX,PY,Int_sell1),
	Intentions1 = intents(Int_sell1,Int_pick).

incorporate_goals(goals(Goals_rest,Goals_truff),beliefs(at(PX,PY),stock(_)), intents(Int_sell,Int_pick), Intentions1):-
	allcontains(Goals_rest,Int_sell),
	\+allcontains(Goals_truff,Int_pick),
	rebuild(Goals_truff,Int_pick,PX,PY,Int_pick1),
	Intentions1 = intents(Int_sell,Int_pick1).

incorporate_goals(goals(Goals_rest,Goals_truff),beliefs(at(PX,PY),stock(_)), intents(Int_sell,Int_pick), Intentions1):-
	\+allcontains(Goals_rest,Int_sell),
	\+allcontains(Goals_truff,Int_pick),
	rebuild(Goals_rest,Int_sell,PX,PY,Int_sell1),
	rebuild(Goals_truff,Int_pick,PX,PY,Int_pick1),
	Intentions1 = intents(Int_sell1,Int_pick1).

% get_action(Beliefs, Intentions, Intentions1, Action)

makeplan(LA,PX,PY,X,Y,Plan):-
	PX>X,
	NX is PX - 1,
	makeplan(LA,NX,PY,X,Y,Subplan),
	Plan = [move(NX,PY)|Subplan].
makeplan(LA,PX,PY,X,Y,Plan):-
	PX<X,
	NX is PX + 1,
	makeplan(LA,NX,PY,X,Y,Subplan),
	Plan = [move(NX,PY)|Subplan].
makeplan(LA,PX,PY,X,Y,Plan):-
	PY<Y,
	NY is PY + 1,
	makeplan(LA,PX,NY,X,Y,Subplan),
	Plan = [move(PX,NY)|Subplan].
makeplan(LA,PX,PY,X,Y,Plan):-
	PY>Y,
	NY is PY - 1,
	makeplan(LA,PX,NY,X,Y,Subplan),
	Plan = [move(PX,NY)|Subplan].
makeplan(LA,PX,PY,X,Y,LA):-
	PY=:=Y,
	PX=:=X.

numberoflelements([_|Tail],Num):-
	numberoflelements(Tail,Num1),
	Num is Num1+1.
numberoflelements([],0).


get_action(beliefs(at(PX,PY),stock(T)),intents([[goal(X,Y,S),SellPlan]|SellTail],Int_pick),intents([[goal(X,Y,S),NNextactions]|SellTail],Int_pick),NFirstaction):-
	S=<T,
	(numberoflelements(SellPlan,0);SellPlan = [SF|_],\+applicable(SF)),
	makeplan([sell(X,Y)],PX,PY,X,Y,[NFirstaction|NNextactions]).

get_action(beliefs(at(_,_),stock(T)),intents(Int_sell,Int_pick),NewIntention,Firstaction):-
	Int_sell = [SellHead|SellTail],
	SellHead = [goal(X,Y,S),SellPlan],
	S=<T,
	SellPlan = [Firstaction|Nextactions],
	NSellHead = [goal(X,Y,S),Nextactions],
	Int_sell1 = [NSellHead|SellTail],
	NewIntention = intents(Int_sell1,Int_pick).
get_action(beliefs(at(PX,PY),stock(T)),intents(Int_sell,Int_pick),NewIntention,NFirstaction):-
	(numberoflelements(Int_sell,0);
	Int_sell = [SellHead|_],
	SellHead = [goal(_,_,S),_],
	S>T),
	Int_pick = [PickHead|PickTail],
	PickHead = [goal(X,Y,Z),PickPlan],
	(numberoflelements(PickPlan,0);PickPlan = [PF|_],\+applicable(PF)),
	makeplan([pick(X,Y)],PX,PY,X,Y,[NFirstaction|NNextactions]),
	NPickHead = [goal(X,Y,Z),NNextactions],
	Int_pick1 = [NPickHead|PickTail],
	NewIntention = intents(Int_sell,Int_pick1).

get_action(beliefs(at(_,_),stock(T)),intents(Int_sell,Int_pick),NewIntention,Firstaction):-
	(numberoflelements(Int_sell,0);
	Int_sell = [SellHead|_],
	SellHead = [goal(_,_,S),_],
	S>T),
	Int_pick = [PickHead|PickTail],
	PickHead = [goal(X,Y,Z),PickPlan],
	PickPlan = [Firstaction|Nextactions],
	NPickHead = [goal(X,Y,Z),Nextactions],
	Int_pick1 = [NPickHead|PickTail],
	NewIntention = intents(Int_sell,Int_pick1).

get_action(beliefs(at(X,Y),stock(T)),intents(Int_sell,Int_pick),intents(Int_sell,Int_pick),move(X,Y)):-
	(numberoflelements(Int_sell,0);
	Int_sell = [SellHead|_],
	SellHead = [goal(_,_,S),_],
	S>T),
	numberoflelements(Int_pick,0).


update_beliefs(at(X,Y), beliefs(at(_,_),stock(T)), beliefs(at(X,Y),stock(T))).
update_beliefs(picked(PX,PY,S),beliefs(at(PX,PY),stock(T)),beliefs(at(PX,PY),stock(T1))):-
	T1 is T + S.
update_beliefs(sold(PX,PY,S),beliefs(at(PX,PY),stock(T)),beliefs(at(PX,PY),stock(T1))):-
	T1 is T - S.

update_intentions(at(_,_),Intentions,Intentions).
update_intentions(sold(_,_,_),intents([_|SellTail],Int_pick),intents(SellTail,Int_pick)).
update_intentions(picked(_,_,_),intents(Int_sell,[_|PickTail]),intents(Int_sell,PickTail)).
