p(a,1).
p(b,-2).

q(c,-3) :- not q(d,4).
q(d,4) :- not q(c,-3).

b(-5).
b(-4).
b(-3).
b(-2).
b(-1).
b(0).
b(1).
b(2).
b(3).
b(4).

a :- #sum { W,X: p(X,W) } != -2.
b :- #sum { W,X: p(X,W) } != -1.
c :- #sum { W,X: p(X,W) } != 0.

d(B) :- #sum { W,X: p(X,W); W,X: q(X,W) } != B, b(B).
f(B) :- b(B), not d(B).
g(B) :- b(B), not f(B).
