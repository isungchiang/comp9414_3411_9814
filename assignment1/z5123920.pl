% full name: yisong jiang
% student number: z5123920
% assignment name: Assignment 1 - Prolog Programming
% course: COMP9814

% weird_sum(List, Result)
% use four predicates to represent different situations of the target number

weird_sum([],0).

weird_sum([Head|Tail],Result) :-
	Head >= 5,
	weird_sum(Tail, RestResult) ,
	Result is RestResult + Head * Head .

weird_sum([Head|Tail],Result) :-
	Head < 5,
	Head > 2,
	weird_sum(Tail, RestResult) ,
	Result is RestResult.

weird_sum([Head|Tail],Result) :-
	Head =< 2,
	weird_sum(Tail, RestResult) ,
	Result is RestResult - abs(Head).


% same_name(Person1,Person2)
% same person has same name if they are same person.
% Two people have same name if one person has same name as another persons father or other words one is anothers father

% I check the male or female of Person1 , so that the people not in the original file will return false as needed.

same_name(Person1,Person1):-
	male(Person1).
same_name(Person1,Person1):-
	female(Person1).

same_name(Person1,Person2) :-
	parent(Person3, Person1),
	male(Person3),
	same_name(Person3,Person2).

same_name(Person1,Person2) :-
	parent(Person3, Person2),
	male(Person3),
	same_name(Person3,Person1).
    

% log_table(NumberList, ResultList)
% binds ResultList to the list of pairs consisting of a number and its log, for each number in NumberList
% each log function will produce a new sublist, just let the list to be the head of total result list

log_table([],[]).

log_table([Head|Tail],ResultList) :-
	log_table(Tail, RestResultList),
	Node is log(Head),
	SubList = [Head, Node],
	ResultList = [SubList|RestResultList].

% function_table(+N, +M, +Function, -Result)
% binds Result to the list of pairs consisting of a number X and Function(X), from N down to M.
% use function round to leave only 5digits in the result.
% each calculate will produce a new list, let the new list be the head.

function_table(N , M, _, []) :-
	N < M.

function_table(N, M, Function, Result) :-
	N >= M,
	function_table(N-1, M, Function, RestResult),
	X =.. [Function, N],
	Y is round(X * 100000) / 100000,
	Z is N + 0,
	SubList = [Z, Y],	
	Result = [SubList|RestResult].

% paruns(List, RunList)
% sametype will find the longest sub list that is odd/even same as first element, the rest list will be returned too.

sametype([],_,[],[]).

sametype([Head|Tail], Smod ,Result, ListTail) :-
	M1 is Head mod 2,
	M1 =:= Smod,
	sametype(Tail, Smod, RestResult, ListTail),
	Result = [Head|RestResult].

sametype([Head|Tail], Smod , [], [Head|Tail]) :-
	M1 is Head mod 2,
	M1 =\= Smod.

% paruns will first find several numbers that have same (mod 2) as the fisrt number through sametype function build above.
% The ListTail which returned from sametype function is the rest numbers in original list.
% do the same thing to the ListTail.

paruns([],[]).

paruns([Head|Tail],RunList) :-
	M1 is Head mod 2,
	sametype([Head|Tail], M1, Result, ListTail),
	paruns(ListTail,RestResult),
	RunList = [Result|RestResult].

% is_heap(tree)
% predicate nodevalue will return the value of tree.
% Search from the root, root value must smaller than left value and right value.
% Amusme the Left tree and Right tree as a individual tree.
% Do the same thing to Left and Right as root and keep do that on their child trees.
% if any trees if not heap, then the original tree is not heap.

nodevalue(tree(_, Number,_), Number).

is_heap(empty).

is_heap(tree(empty, _, empty)).

is_heap(tree(Left , Number, Right)) :-
	nodevalue(Left,LeftValue),
	nodevalue(Right,RightValue),
	Number =< LeftValue,
	Number =< RightValue,
	is_heap(Left),
	is_heap(Right).

is_heap(tree(Left , Number, empty)) :-
	nodevalue(Left,LeftValue),
	Number =< LeftValue,
	is_heap(Left).

is_heap(tree(empty , Number, Right)) :-
	nodevalue(Right,RightValue),
	Number =< RightValue,
	is_heap(Right).






% max(A, B, C) binds C to the larger of A and B.
max(A, B, A) :-
	A > B.

max(A, B, B) :-
	A =< B.

% build predicate height_of_node to return the height of one treenode.

height_of_node(empty, 0).

height_of_node(tree(Left, _, Right), Height) :-
	height_of_node(Left,LH),
	height_of_node(Right,RH),
	max(LH,RH,Bigger),
	Height is Bigger + 1.

% height_if_balanced(Tree,HiB).

height_if_balanced(empty,0).

% if one tree has unbalanced left or right height then return -1.

height_if_balanced(tree(Left, _, Right), -1) :-
	height_of_node(Left,LH),
	height_of_node(Right,RH),
	Self is abs(LH-RH),
	Self >= 2.

% if one tree has balanced left and right height ,then check its child tree.
% if its child trees are balanced too, then the height is the bigger one.

height_if_balanced(tree(Left, _, Right), HiB) :-
	height_of_node(Left,LH),
	height_of_node(Right,RH),
	Self is abs(LH-RH),
	Self < 2,
	height_if_balanced(Left, HiBL),
	height_if_balanced(Right, HiBR),
	HiBL >= 0,
	HiBR >= 0,
	max(LH +1 ,RH + 1,NHiB),
	HiB is NHiB.

% the three predicates below describes the three situations that one tree has unbalanced child tree
% since its child tree is not balanced, the HiB will be returned as -1.

height_if_balanced(tree(Left, _, Right), -1) :-
	height_of_node(Left,LH),
	height_of_node(Right,RH),
	Self is abs(LH-RH),
	Self < 2, 
	height_if_balanced(Left, HiBL),
	height_if_balanced(Right, HiBR),
	HiBL =:= -1,
	HiBR >= 0.

height_if_balanced(tree(Left, _, Right), -1) :-
	height_of_node(Left,LH),
	height_of_node(Right,RH),
	Self is abs(LH-RH),
	Self < 2,
	height_if_balanced(Left, HiBL),
	height_if_balanced(Right, HiBR),
	HiBR =:= -1,
	HiBL >= 0.

height_if_balanced(tree(Left, _, Right), -1) :-
	height_of_node(Left,LH),
	height_of_node(Right,RH),
	Self is abs(LH-RH),
	Self < 2,
	height_if_balanced(Left, HiBL),
	height_if_balanced(Right, HiBR),
	HiBR =:= -1,
	HiBL =:= -1.


