% facts zero and infinity, used in food storage
zero(0.0).
infinity(999999.0).

% builds a complete graph
buildEdges(Locs, Cap, R) :-
    buildEdges(Locs, Locs, Cap, R).
buildEdges([_], _, _, []) :- !.
buildEdges([H1,H2|RemLocs], AllLocs, Cap, Result) :-
    buildEdgesForVertex(H1, [H2|RemLocs], AllLocs, Cap, R1),
    buildEdges([H2|RemLocs], AllLocs, Cap, R2),
    append(R1,R2,Result).

% builds all edges for a single vertex
buildEdgesForVertex(L, Locs, Cap, R) :-
	buildEdgesForVertex(L, Locs, Locs, Cap, R).
buildEdgesForVertex(_, [], _, _, []) :- !.
buildEdgesForVertex(L, [H|RemLocs], AllLocs, Cap, [(X, Y, W)|R]) :-
	getIndex(AllLocs, L, X),
	getIndex(AllLocs, H, Y),
	getDistance(L, H, W),
	buildEdgesForVertex(L, RemLocs, AllLocs, Cap, R).

% gets food storage, food storage per location
getFoodStorage(Locs, R) :-
    getFoodStorage(Locs, Locs, R).
getFoodStorage([], _, []) :- !.
getFoodStorage([H1|RemLocs], AllLocs, [(I,S)|Result]) :-
    getIndex(AllLocs, H1, I),
    (RemLocs \= [] -> infinity(S); zero(S)),
    getFoodStorage(RemLocs, AllLocs, Result).

checkStorage(Stor, R) :-
    checkStorage(Stor, 0, R).
checkStorage([(I1,S)|Rem], I2, R) :-
    (I1 == I2 -> R is S; checkStorage(Rem, I2, R)).

% updates the food stored at location X.
updateStorage([], _, _, []) :- !.
updateStorage([(X1, F1)|Rem], I, V, [(X2,F2)|Result]) :-
    (X1 == I, V < F1 -> F2 is V; F2 is F1),  
    X2 is X1,
    updateStorage(Rem, I, V, Result).

% calculates the amount of food required to cross the desert.
bfs([], Stor, Cap, Food) :-
    checkStorage(Stor, Food).
bfs([_], [], Cap, Food) :-
    infinity(Food).
bfs([(X,Y,D)|RemEdges], Stor, Cap, Food) :-
    (2*D >= Cap ->
        bfs(RemEdges, Stor, Cap, Food);
        checkStorage(Stor, Y, Res),

        (Res+2*D =< Cap ->                          % 2 * dis (water AND food)
            checkStorage(Stor, Y, Res2),            
            updateStorage(Stor, X, Res2 + D, UpdStor),
            bfs(RemEdges, UpdStor, Cap, Food);
            
            (3*D >= Cap ->                          % (2 * dis) + (1 * dis)
                bfs(RemEdges, Stor, Cap, Food);     % terminate, return 0 or -1

                checkStorage(Stor, Y, S1),
                
                Dis3 is 3*D,                        % 3 * dis
                CapMinDis3 is Cap - Dis3,           % cap - (dis * 3)
                CE is ceiling(CapMinDis3/S1),       % (cap - (dis * 3)) / storage

                A1 is Cap - D,
                A2 is CE,
                A3 is S1 + D - CapMinDis3 * CE,

                updateStorage(Stor, X, A1 * A2 + A3, UpdStor2),

                bfs(RemEdges, UpdStor2, Cap, Food)
            )
        )
    ).

reverse(X, Y) :- reverse(X, [], Y).
reverse([X|Y], Z, W) :- reverse(Y, [X|Z], W).
reverse([], X, X).

getIndex([E|_], E, 0) :- !.
getIndex([_|T], E, I):- getIndex(T, E, I1), incr(I1, I).

incr(X, X1) :- X1 is X+1.
decr(X, X1) :- X1 is X-1.

% calculates the distance between two vertices
getDistance((X1, Y1), (X2, Y2), D) :- hypotenuse(X2-X1, Y2-Y1, D).

% calculates the hypotenuse of a right triangle
hypotenuse(L1, L2, H) :- sqrt(L1^2 + L2^2, H).

% reads locations from the standard input.
readLocations(Cap, Locs) :-
    writeln('Enter the total number of locations (e.g. 4):'),
    read(O),
    (O < 2 -> 
    	( O == 0 ->
    		Cap is -1,
    		!;
    		writeln('Enter a number greater than 1 or enter a 0 to terminate:'),
    		readLocations(Cap, Locs)
    	);
    	writeln('Enter the maximum capacity (e.g. 100):'),
   	 	read(Cap),
    	getLocations(Locs, O)
    ).

getLocations(Locs, N) :-
    writeln('Enter the location of an oasis (e.g. (10,-20).):'),
    read(L),
    addLocation(L, Locs, N).

addLocation(L, [L|Locs], N) :-
    decr(N, N1),
    (N1 == 0 -> !; getLocations(Locs,N1)).

main :-
    writeln('New trial'),	
	readLocations(Cap, Locs),
	
    writeln(''),

	(Cap \= -1 ->
        buildEdges(Locs, Cap, Edges),
		getFoodStorage(Locs, FoodStor),
		reverse(Edges, RevEdges),
		bfs(RevEdges, FoodStor, Cap, FoodRes),
        infinity(I),
		FoodReq is ceiling(FoodRes),
		(FoodReq \= I ->
			writeln('Units of food required to cross the desert:'),
			writeln(FoodReq);

            writeln('Impossible')
        ),

        writeln(''),
		main;
        
        writeln('Terminating...'),
        !
    ).