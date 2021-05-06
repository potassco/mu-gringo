p(1).
p(2).
q(3) :- not r(3).
r(3) :- not q(3).

a :- #count { X: p(X) } != 1.
b :- #count { X: p(X) } != 2.
c :- #count { X: p(X) } != 3.
d :- #count { X: p(X); X: q(X) } != 3.
