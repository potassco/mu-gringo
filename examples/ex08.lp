p(a,-1).
p(b,-2).
p(c,-3).
q(c,-4) :- not r(c,-4).
r(c,-4) :- not q(c,-4).
q(c,-5) :- not r(c,-5).
r(c,-5) :- not q(c,-5).

b(-9).
b(-10).
b(-11).
b(-12).
b(-13).
b(-14).
b(-15).
b(-16).

a :- #sum- { W,X: p(X,W) } != -5.
b :- #sum- { W,X: p(X,W) } != -6.
c :- #sum- { W,X: p(X,W) } != -7.
d(B) :- #sum- { W,X: p(X,W); W,X: q(X,W) } != B, b(B).
f(B) :- b(B), not d(B).
