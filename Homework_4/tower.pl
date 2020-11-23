% N: size of square
% T: List of all the rows as lists
% C: structure with counts

tower(N,T,C) :-
	% Check length of rows compared to N
	length(T,N),
	row_length(T,N),
	% Make sure numbers are valid
	in_domain(T,N),
	maplist(fd_all_different,T),
	transpose(T,TT),
	maplist(fd_all_different,TT),
	maplist(fd_labeling,T),
	reverse_2Dlist(T,RT),
	reverse_2Dlist(TT,RTT),
	% Pull the counts and check their lengths
	counts(UP,DOWN,LEFT,RIGHT) = C,
	length(UP, N),length(DOWN, N),length(LEFT, N),length(RIGHT, N),
	check(T,LEFT), check(RT,RIGHT), check(TT,UP), check(RTT,DOWN).

plain_tower(N,T,C) :-
	length(T,N),
	row_length(T,N),
	verify_permutations(T,N),
	maplist(unique, T),
	transpose(T,TT),
	maplist(unique, TT),
	reverse_2Dlist(T,RT),
	reverse_2Dlist(TT,RTT),
	% Pull the counts and check their lengths
	counts(UP,DOWN,LEFT,RIGHT) = C,
	length(UP, N),length(DOWN, N),length(LEFT, N),length(RIGHT, N),
	check(T,LEFT), check(RT,RIGHT), check(TT,UP), check(RTT,DOWN).

tower_time(TIME) :-
    statistics(cpu_time,[START|_]),
    tower(4, T, counts([3,1,2,2],[2,3,1,3],[2,3,1,2],[3,1,2,2])),
    statistics(cpu_time,[STOP|_]),
    TIME is STOP - START.

plain_tower_time(TIME) :-
    statistics(cpu_time,[START|_]),
    plain_tower(4, T, counts([3,1,2,2],[2,3,1,3],[2,3,1,2],[3,1,2,2])),
    statistics(cpu_time,[STOP|_]),
    TIME is STOP - START.

speedup(RATIO) :- 
    tower_time(X),
    plain_tower_time(Y), 
    RATIO is Y/X.

ambiguous(N,C,T1,T2) :-
	tower(N,T1,C),
	tower(N,T2,C),
	T1 \= T2.

% Test if elements in a list are unique, sort merges duplicate elements
unique(LIST) :- 
	sort(LIST,SORTED),
	length(LIST,L1),
	length(SORTED,L2),
	L1 == L2.

% Check each elem is between 1 and N inclusive, using fd domain
in_domain([],_).
in_domain([HD|TL],N) :-
	fd_domain(HD,1,N),
	in_domain(TL,N).

% Check that each row is length N
row_length([], _).
row_length([HD|TL],N) :- 
	length(HD,N), 
	row_length(TL,N).

% Flips every row in a 2D list, allows us to go left to right always
reverse_2Dlist([],[]).
reverse_2Dlist([A | B], [C | D]) :-
	reverse(A,C),
	reverse_2Dlist(B,D).

% Checks the 2D list with one solution side 
% Assumes we are going left to right
check([],[]).
check([ROW | ROWTAIL],[COUNTHD | COUNTTAIL]) :-
	check_row(ROW,COUNTHD),
	check(ROWTAIL,COUNTTAIL).

% Checks just a row given a count
check_row([ELEM | RESTELEMS],COUNT):-
	check_row(RESTELEMS,ELEM,1,COUNT).

check_row([],_,V,V). % finally return true if count is the same as view
check_row([CELEM | RESTELEMS],MAX,VIEW,COUNT) :-
	CELEM > MAX,
	NVIEW is VIEW+1,
	check_row(RESTELEMS,CELEM,NVIEW,COUNT).
check_row([CELEM | RESTELEMS], MAX, VIEW, COUNT) :-
	CELEM < MAX,
	check_row(RESTELEMS,MAX,VIEW,COUNT).

verify_permutations([],_).
verify_permutations([ROW | ROWREST],N) :-
	findall(A,between(1,N,A),PERM),
	permutation(PERM,ROW),
	verify_permutations(ROWREST,N).

% SWI Transpose
transpose([], []).
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
	lists_firsts_rests(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).
	
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
	lists_firsts_rests(Rest, Fs, Oss).
