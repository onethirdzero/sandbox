% Tutorial found here:
% https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/

% relationship(arg1, arg2, ...).
likes(alice, bob).
likes(bob, carol).
likes(james, mary).
likes(mary, james).

% conclusion(arg1, arg2, ...) :- relation1, relation2, ... , relationN.
love_compatible(X, Y) :- likes(X, Y), likes(Y, X).

mother(alice, lea).
mother(john, julia).
mother(lea, alberta).
father(james, alfred).
father(lea, john).

% To Prolog, it doesn't matter if there are multiple definitions of a rule.
% If the first clause fails, it just tries the second one, and so on.
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

% For Prolog, there is no difference between a rule and a fact.
% A fact is a rule that is always true.

% A rule can be defined in terms of another rule.

% If X's parent is Z and Z's parent is Y, then X's grandparent is Y.
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
