yellow(banana).

likes(bob, apple).
likes(bob, grape).

likes(alice, apple).
likes(alice, banana).

likes(ronald, X) :- yellow(X), likes(alice, X).

append(nil, M, M).

append(cons(H, T), X, cons(H, R)) :- append(T, X, R).