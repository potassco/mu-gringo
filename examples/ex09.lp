p(a,1).
p(b,-2).
p(c,3).
q(c,-4) :- not r(c,-4).
r(c,-4) :- not q(c,-4).
q(c,5) :- not r(c,5).
r(c,5) :- not q(c,5).

b(-3).
b(-2).
b(7).
b(8).

a :- #sum { W,X: p(X,W) } >= 1.
b :- #sum { W,X: p(X,W) } >= 2.
c :- #sum { W,X: p(X,W) } >= 3.
d :- #sum { W,X: p(X,W) } <= 1.
e :- #sum { W,X: p(X,W) } <= 2.
f :- #sum { W,X: p(X,W) } <= 3.

g(B) :- #sum { W,X: p(X,W); W,X: q(X,W) } >= B, b(B).
h(B) :- #sum { W,X: p(X,W); W,X: q(X,W) } <= B, b(B).
fh(B) :- b(B), not g(B).
gh(B) :- b(B), not h(B).
