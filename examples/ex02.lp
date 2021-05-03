company(c1).
company(c2).
company(c3).
company(c4).

owns(c1,c2,60).
owns(c1,c3,20).
owns(c2,c3,35).
owns(c3,c4,51).

controls(X,Y) :- #sum+ { S: owns(X,Y,S);
                         S,Z: controls(X,Z), owns(Z,Y,S) } > 50,
                 company(X), company(Y), X!=Y.
