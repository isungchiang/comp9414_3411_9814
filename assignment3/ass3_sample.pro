
% ########################################QUESTION 1#################################################

% Write a Prolog procedure trigger(Events, Goals) which takes a list of events, each of the form junk(X,Y,S), 
% and computes the corresponding list of goals for the agent, each of the form goal(X,Y,S).


% When calling copy, works its way down to the base case, setting Goals to an empty list.
% Then works itself back up and keeps appending the head H of known list [H|T1] to the
% beginning of variable list [H|T2], continuing until the original case is reached, where
% the list Goals will contain a full copy of the list Events:

% Base case

trigger([], []).
trigger([junk(X, Y, S)|T], [goal(X, Y, S)|G]) :-
    trigger(T, G).


% #########################################QUESTION 2################################################

% Write a Prolog procedure: incorporate_goals(Goals, Beliefs, Intentions, Intentions1).
% There are four arguments: 
% 1) a list of goals each of the form goal(X,Y,S)
% 2) a list of beliefs (containing one term of the form at(X,Y))
% 3) the current list of intentions each of the form [goal(X,Y,S), Plan]
% 4) a list to be computed which contains the new goals inserted into the current list of intentions in increasing
%    order of distance, using the value of the item to break ties. More precisely, a new goal should be placed immediately 

%    before the first goal in the list that is further away from the agent's current position, or which is at the same distance
%    from the agent but of lower value, without reordering the current list of goals.
% Note that because of repeated perception of the same event, only new goals should be inserted into the list of intentions. The
% plan associated with each new goal should be the empty plan (represented as the empty list).


insert_goal(Goal, [Intent|Intentions], Belief, [Intent|Intentions1]):-
    not(compare_goal(Goal, Intent, Belief)), !, 
    insert_goal(Goal, Intentions, Belief, Intentions1).

insert_goal(X, Intentions, _, [[X, []]|Intentions]).

% check if is a member

is_member(Goal, [Head|_]) :-
    member(Goal, Head).

is_member(Goal, [Head|Tail]) :-
    not(member(Goal, Head)),
	is_member(Goal, Tail).

% compare goal, which is prior.


% Compare scores.
compare_goal(goal(_, _, S1), [goal(_, _, S2)|_], _) :-
    S1 > S2.

% Compare distances to Belief.
compare_goal(goal(X1, Y1, S1), [goal(X2, Y2, S2)|_], [at(X, Y)|_]) :-
    S1 == S2,
    distance((X, Y), (X1, Y1), D1),
    distance((X, Y), (X2, Y2), D2),
    D1 < D2.

% Base case.
incorporate_goals([], _, Intentions, Intentions).

% Goal is already in the Intentions.
incorporate_goals([Goal|Tail], Belief, Intentions, Intentions1) :-
    is_member(Goal, Intentions),
    incorporate_goals(Tail, Belief, Intentions, Intentions1).

% not in the Intentions list.
incorporate_goals([Goal|Tail], Belief, Intentions, Intentions1) :-
    not(is_member(Goal, Intentions)),
    insert_goal(Goal, Intentions, Belief, UpdatedIntentions),
    incorporate_goals(Tail, Belief, UpdatedIntentions, Intentions1).

%   Insert the Goal in Intention .



% #########################################QUESTION 3################################################

% Write a Prolog procedure, select_action(Beliefs, Intentions, Intentions1, Action), which takes the agent's beliefs (a singleton
% list containing a term for the agent's location) and the list of intentions, and computes an action to be taken by the agent 
% and the updated list of intentions.
% The intention selected by the agent is the first on the list of intentions (if any). If the first action in this plan is 
% applicable, the agent selects this action and updates the plan to remove the selected action. If there is no associated plan 
% (i.e. the plan is the empty list) or the first action in the plan for the first intention is not applicable in the current state, 
% the agent constructs a new plan to go from its current position to the goal state and pick up the junk there (this plan will be a 
% list of move actions followed by a pick up action), selects the first action in this new plan, and updates the list of intentions 
% to incorporate the new plan (minus the selected first action). 
% Due to the fact that there are no obstacles in the world, the exact path the agent takes towards the goal does not matter, so 
% choose any convenient way of implementing this procedure. The procedure applicable is defined in gridworld.pro

% If the intentions are empty move towards (5,5) and two-step.
select_action([at(5, 5)], [], [], move(6, 5)).
select_action([at(X, Y)], [], [], move(Xnew, Ynew)) :-
    X < 5, Xnew is X + 1, Ynew = Y;
    X > 5, Xnew is X - 1, Ynew = Y;
    Y < 5, Ynew is Y + 1, Xnew = X;
    Y > 5, Ynew is Y - 1, Xnew = X.

% Action is applicable.
select_action(Beliefs, [Intent|Tail], [[Goal, NextActions]|Tail], Action) :-
    d_intention(Intent, Goal, [Action|NextActions]),
    applicable(Beliefs, Action).

% Action is not applicable.
select_action(Beliefs, [Intent|Tail], [[Goal, Plan]|Tail], Action) :-
    d_intention(Intent, Goal, [BadAction|_]),
    not(applicable(Beliefs, BadAction)),
    new_plan(Goal, Beliefs, NewPlan),
    next_action(NewPlan, Plan, Action).



next_action([Action|Plan], Plan, Action).


%  get Goal and Plan  <= intention

d_intention([Goal|Plan], Goal, Plan).


%   Generate a list of move() actions ending with a pickup() action based on
%   where the robot is at() currently.

new_plan(Goal, Beliefs, Plan) :-
    new_plan(Goal, Beliefs, [], Plan).

new_plan(goal(X, Y, _), [at(X, Y)], PartialPlan, Plan) :-
    reverse([pickup(X, Y)|PartialPlan], Plan).

new_plan(Goal, [at(X, Y)], PartialPlan, Plan) :-
    valid_move(X, Y, move(Xnew, Ynew)),
    heuristic(move(Xnew, Ynew), Goal, at(X, Y)),
    new_plan(Goal, [at(Xnew, Ynew)], [move(Xnew, Ynew)|PartialPlan], Plan).
%reverse the list, from google

reverse(List, Reversed) :-
    reverse(List, [], Reversed).

reverse([], Reversed, Reversed).

reverse([X|Rest], PartReversed, TotalReversed) :-
    reverse(Rest, [X|PartReversed], TotalReversed).


valid_move(X, Y, Next_Move) :-
    Dx is X + 1, Next_Move = move(Dx, Y);
    Dx is X - 1, Next_Move = move(Dx, Y);
    Dy is Y + 1, Next_Move = move(X, Dy);
    Dy is Y - 1, Next_Move = move(X, Dy).


%   Heuristic function to determine weather a Move is in the right direction
%   or not. Since the move distance can only be 1 or -1 this is more or
%   less boolean. More specifically, move has to be closer to the Goal than
%   current robot position.

heuristic(move(X, Y), goal(Xgoal, Ygoal, _), at(X2, Y2)) :-
    distance((X, Y), (Xgoal, Ygoal), D1),
    distance((X2, Y2), (Xgoal, Ygoal), D2),
    D1 < D2.


%   Determine all valid moves for a given X, Y coordinate.






% #########################################QUESTION 4################################################
% Write two Prolog procedures:
% 1) update_beliefs(Observation, Beliefs, Beliefs1)
% 2) update_intentions(Observation, Intentions, Intentions1)
% Compute the lists of beliefs and intentions resulting from the agent's observations.
% These are very simple procedures (one line for each possible observation type)!

% update_beliefs(+Observation, @Beliefs, -Beliefs1).
%   Update robots Beliefs based on Observations. Replace the old at()
%   with the new at().

update_beliefs(at(X, Y), _, [at(X,Y)]).

% ignore cleaned() observations.
update_beliefs(_, Beliefs, Beliefs).



%   Update intentions based on Observations. Remove the goal once the junk has
%   been cleaned. Assuming its still the last goal to have been reached.

update_intentions(cleaned(X, Y), [[goal(X, Y, _)|_]|Intentions1], Intentions1).

% catch the rest to stop backtracking.
update_intentions(_, Intentions, Intentions).

