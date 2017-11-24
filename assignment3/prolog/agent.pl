% COMP9814 Assignment3
% Option 2: Prolog (BDI Agent)
% Group14: Yisong Jiang(z5123920) Yangyi Huang (z5015832)


% trigger(+Events, -Goals)
% 	Insert every event to either a sell goal list or a pick goal list.
%	Just need to check each event if it is a truffle or a restaurant.
trigger([], goals([],[])).

trigger([truffle(X,Y,S)|Restpercepts],goals(RGoals_rest,[goal(X,Y,S)|RGoals_truff])):-
	trigger(Restpercepts,goals(RGoals_rest,RGoals_truff)).

trigger([restaurant(X,Y,S)|Restpercepts],goals([goal(X,Y,S)|RGoals_rest],RGoals_truff)):-
	trigger(Restpercepts,goals(RGoals_rest,RGoals_truff)).

% incorporate_goals(+Goals, +Beliefs, +Intentions, -Intentions1)
% 	This procedure need other procedure to process,including procedures rebuild, contains and allcontains, insertgoal. 
% 	We check if all the goals are in the initial intents list.
% 	If Int_sell doesnt contains all the elements in Goals_rest, then we go to rebuild Int_sell.
% 	If Int_pick doesnt contains all the elements in Goals_truff, then we go to rebuild Int_pick.
% 	If both, then rebuild both of them.
incorporate_goals(goals(Goals_rest,Goals_truff),_, intents(Int_sell,Int_pick), intents(Int_sell,Int_pick)):-
	allcontains(Goals_rest,Int_sell),
	allcontains(Goals_truff,Int_pick).

incorporate_goals(goals(Goals_rest,Goals_truff),beliefs(at(PX,PY),stock(_)), intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick)):-
	\+allcontains(Goals_rest,Int_sell),
	allcontains(Goals_truff,Int_pick),
	rebuild(Goals_rest,Int_sell,PX,PY,Int_sell1).

incorporate_goals(goals(Goals_rest,Goals_truff),beliefs(at(PX,PY),stock(_)), intents(Int_sell,Int_pick), intents(Int_sell,Int_pick1)):-
	allcontains(Goals_rest,Int_sell),
	\+allcontains(Goals_truff,Int_pick),
	rebuild(Goals_truff,Int_pick,PX,PY,Int_pick1).

incorporate_goals(goals(Goals_rest,Goals_truff),beliefs(at(PX,PY),stock(_)), intents(Int_sell,Int_pick), intents(Int_sell1,Int_pick1)):-
	\+allcontains(Goals_rest,Int_sell),
	\+allcontains(Goals_truff,Int_pick),
	rebuild(Goals_rest,Int_sell,PX,PY,Int_sell1),
	rebuild(Goals_truff,Int_pick,PX,PY,Int_pick1).

% rebuild(+Goals,+Intentions,+Xposition,+Yposition,-Intentions1)
% 	Rebuild procedure will search the goals in Goals_rest or Goals_truff that are not
% 	in the initial intentions. Insert them into the intentions and return as the Intentions1.
rebuild([],Int,_,_,Int).

rebuild([goal(X,Y,Z)|Tail],Int,PX,PY,Int1):-
	contains(goal(X,Y,Z),Int),
	rebuild(Tail,Int,PX,PY,Int1).

rebuild([goal(X,Y,Z)|Tail],Int,PX,PY,Int1):-
	\+contains(goal(X,Y,Z),Int),
	rebuild(Tail,Int,PX,PY,Int2),
	insertgoal(goal(X,Y,Z),Int2,PX,PY,Int1).

% contains(+Goal,+Int[]) 
% 	Check if a goal is in the intention list.
contains(goal(X,Y,_),[[goal(X,Y,_),_]|_]).
contains(goal(X,Y,_),[[goal(X1,Y1,_),_]|Tail]):-
	X1 \= X,
	Y1 \= Y,
	contains(goal(X,Y,_),Tail).
contains(goal(X,Y,_),[[goal(X1,Y1,_),_]|Tail]):-
	X1 =:= X,
	Y1 \= Y,
	contains(goal(X,Y,_),Tail).
contains(goal(X,Y,_),[[goal(X1,Y1,_),_]|Tail]):-
	X1 \= X,
	Y1 =:= Y,
	contains(goal(X,Y,_),Tail).

% allcontains(+Goal[],+Int[])
%	A updated version of procedure contains.
%	It will check if all the goals in a list is a member of intention list.
allcontains([],_).

allcontains([Head|Tail],Checklist):-
	contains(Head,Checklist),
	allcontains(Tail,Checklist).

% insertgoal(+Goal, +Int[], +Xposition, +Yposition, -Intentions1)
% 	Insert a goal to a intention list.
% 	Compare the value of goals with the goal in initial intentions.
% 	If new one has higher value, insert it before the compared one.
% 	If new one has same value and shorter distance, insert it before the compared one.
% 	Else compare new goal with next one.

insertgoal(goal(X,Y,Z),[],_,_,[[goal(X,Y,Z),[]]]).

insertgoal(goal(X,Y,Z),[[goal(X1,Y1,Z1),HPlan]|Tail],_,_,[[goal(X,Y,Z),[]]|[[goal(X1,Y1,Z1),HPlan]|Tail]]):-
	Z>Z1.

insertgoal(goal(X,Y,Z),[[goal(X1,Y1,Z1),HPlan]|Tail],PX,PY,[[goal(X1,Y1,Z1),HPlan]|Int2]):-
	Z<Z1,
	insertgoal(goal(X,Y,Z),Tail,PX,PY,Int2).

insertgoal(goal(X,Y,Z),[[goal(X1,Y1,Z1),HPlan]|Tail],PX,PY,[[goal(X,Y,Z),[]]|[[goal(X1,Y1,Z1),HPlan]|Tail]]):-
	Z=:=Z1,
	distance((X,Y),(PX,PY),NewD),
	distance((X1,Y1),(PX,PY),HeadD),
	NewD < HeadD.

insertgoal(goal(X,Y,Z),[[goal(X1,Y1,Z1),HPlan]|Tail],PX,PY,[[goal(X1,Y1,Z1),HPlan]|Int2]):-
	Z=:=Z1,
	distance((X,Y),(PX,PY),NewD),
	distance((X1,Y1),(PX,PY),HeadD),
	NewD >= HeadD,
	insertgoal(goal(X,Y,Z),Tail,PX,PY,Int2).


% isEmpty(+List)
% 	Check if a list is empty.
isEmpty([]).


% get_action(+Beliefs, +Intentions, -Intentions1, -Action)
% 	If Int_sell has a intention and its value <= our stock, then select this intention.
%		If this intention dont have a plan or Firstaction of the plan is not applicable, then make a new plan for it
% 		Else: go on this plan and take the Firstaction out.
% 	If Int_sell dont have any intention or first intention has higher value than our stock.
%		Then go to check Int_pick, if first intention dont have a plan or Firstaction of the plan is not applicable, then make a new plan for it
%		Else: go on this plan and take the Firstaction out.
%	Else if Int_sell dont have any intention or first intention has higher value than our stock and Int_pick dont have any intention
%		Then the agent stay still.
get_action(beliefs(at(PX,PY),stock(T)),intents([[goal(X,Y,S),SellPlan]|SellTail],Int_pick),intents([[goal(X,Y,S),NNextactions]|SellTail],Int_pick),NFirstaction):-
	S=<T,
	(isEmpty(SellPlan);SellPlan = [SF|_],\+applicable(SF)),
	makeplan([sell(X,Y)],PX,PY,X,Y,[NFirstaction|NNextactions]).

get_action(beliefs(at(_,_),stock(T)),intents([[goal(X,Y,S),[Firstaction|Nextactions]]|SellTail],Int_pick),intents([[goal(X,Y,S),Nextactions]|SellTail],Int_pick),Firstaction):-
	S=<T.

get_action(beliefs(at(PX,PY),stock(T)),intents(Int_sell,[[goal(X,Y,Z),PickPlan]|PickTail]),intents(Int_sell,[[goal(X,Y,Z),NNextactions]|PickTail]),NFirstaction):-
	(isEmpty(Int_sell);
	Int_sell = [[goal(_,_,S),_]|_],
	S>T),
	(isEmpty(PickPlan);PickPlan = [PF|_],\+applicable(PF)),
	makeplan([pick(X,Y)],PX,PY,X,Y,[NFirstaction|NNextactions]).

get_action(beliefs(at(_,_),stock(T)),intents(Int_sell,[[goal(X,Y,Z),[Firstaction|Nextactions]]|PickTail]),intents(Int_sell,[[goal(X,Y,Z),Nextactions]|PickTail]),Firstaction):-
	(isEmpty(Int_sell);
	Int_sell = [[goal(_,_,S),_]|_],
	S>T).

get_action(beliefs(at(X,Y),stock(T)),intents(Int_sell,Int_pick),intents(Int_sell,Int_pick),move(X,Y)):-
	(isEmpty(Int_sell);
	Int_sell = [[goal(_,_,S),_]|_],
	S>T),
	isEmpty(Int_pick).


% makeplan(+LastAction,+Xposition,+Yposition,+XGoalposition,+YGoalposition,-Plan)
% 	Make a plan for current agent state
% 	The agent must move to the target state and then pick/sell.
makeplan(LA,PX,PY,X,Y,[move(NX,PY)|Subplan]):-
	PX>X,
	NX is PX - 1,
	makeplan(LA,NX,PY,X,Y,Subplan).

makeplan(LA,PX,PY,X,Y,[move(NX,PY)|Subplan]):-
	PX<X,
	NX is PX + 1,
	makeplan(LA,NX,PY,X,Y,Subplan).

makeplan(LA,PX,PY,X,Y,[move(PX,NY)|Subplan]):-
	PY<Y,
	NY is PY + 1,
	makeplan(LA,PX,NY,X,Y,Subplan).

makeplan(LA,PX,PY,X,Y,[move(PX,NY)|Subplan]):-
	PY>Y,
	NY is PY - 1,
	makeplan(LA,PX,NY,X,Y,Subplan).

makeplan(LA,PX,PY,X,Y,LA):-
	PY=:=Y,
	PX=:=X.

% update_beliefs(+Observation, +Beliefs, Beliefs1)
% 	at(X,Y) - the agent should believe it is at(X,Y)
% 	picked(X,Y,S) - stock(T) changes to stock(T1) where T1 is T+S
%	sold(X,Y,S) - stock(T) changes to stock(T1) where T1 is T-S
update_beliefs(at(X,Y), beliefs(at(_,_),stock(T)), beliefs(at(X,Y),stock(T))).
update_beliefs(picked(PX,PY,S),beliefs(at(PX,PY),stock(T)),beliefs(at(PX,PY),stock(T1))):-
	T1 is T + S.
update_beliefs(sold(PX,PY,S),beliefs(at(PX,PY),stock(T)),beliefs(at(PX,PY),stock(T1))):-
	T1 is T - S.

% update_intentions(Observation, Intentions, Intentions1)
% 	An at(X,Y) observation should not change the agent's intentions
% 	In the case of a picked() or sold() observation, the agent should remove the corresponding plan from its list of intentions	
update_intentions(at(_,_),Intentions,Intentions).
update_intentions(sold(_,_,_),intents([_|SellTail],Int_pick),intents(SellTail,Int_pick)).
update_intentions(picked(_,_,_),intents(Int_sell,[_|PickTail]),intents(Int_sell,PickTail)).
