p(1).
p(2).
p(3).

a :- #count { X: p(X) } <= 3.
b :- #count { X: p(X) } <= 2.
c :- not a.
d :- not b.
