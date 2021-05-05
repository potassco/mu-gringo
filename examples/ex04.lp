d(1).
d(2).
d(3).

p(X) :- not q(X), d(X).
q(X) :- not p(X), d(X).

a :- #count { X: p(X) } <= 3.
b :- #count { X: p(X) } <= 2.
c :- not a.
d :- not b.
