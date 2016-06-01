% relationship(arg1, arg2, ...).
likes(alice, bob).
likes(bob, carol).
likes(james, mary).
likes(mary, james).

% conclusion(arg1, arg2, ...) :- relation1, relation2, ... , relationN.
love_compatible(X, Y) :- likes(X, Y), likes(Y, X).