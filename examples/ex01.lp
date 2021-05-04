p(X) :- not q(X), u(X).  u(1). u(2).
q(X) :- not p(X), v(X).        v(2).  v(3).

x :- not p(1).
y :- not q(3).
