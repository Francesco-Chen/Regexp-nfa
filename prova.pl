fattoriale_2(N, F):-fattor(N, 1, F).
fattor(0, F, F).
fattor(N, X, F):-N1 is N - 1, Y is N * X, fattor(N1, Y, F).

padre(mario).

nat(0).
nat(N) :- N>0, N2 is N - 1, nat(N2).

accept([I | Is], S) :- delta(S, I, N), accept(Is, N).
accept([], Q) :- final(Q).

initial(q1). final(q2).
delta(q1, a, q2).


recognize(Input) :- initial(S), accept(Input, S).

is_reg(RE):-atomic(RE)|compound(RE).



split(RE, T):-
	RE =.. [H | T],
  H = seq,
	write([H]),
 	write([T| _]).

assertt(FA_Id, RE) :-
  is_regexp(RE),
  assert(initial(FA_Id, RE)).

count_to_10(10) :- write(10), nl.

count_to_10(X) :-
  write(X),nl,
  Y is X + 1,
  count_to_10(Y).

downcount(0, []):- write(0), nl.
downcount(X, [H| T]):-
  write(X),
  write(H),nl,
  Y is X - 1,
  downcount(Y, T).
















is_regexp(RE):-atomic(RE),!.

is_regexp(RE) :-
  compound(RE),
  RE =.. [H| T],
  H = seq,
  write([H]),
  write([T]),!.

is_regexp(RE) :-
  compound(RE),
  RE =.. [H| T],
  H = or,
  write([H]),
  write([T]),!.

is_regexp(RE) :-
  compound(RE),
  RE =.. [H| T],
  H = star,
  write([H]),
  write([T]),!.

is_regexp(RE) :-
  compound(RE),
  RE =.. [H| T],
  H = plus,
  write([H]),
  write([T]),!.





nfa_regexp_comp(FA_Id, RE) :-
  nonvar(FA_Id),
  is_regexp(RE),
  atomic(RE),
  gensym(q, Start),
  assert(nfa_initial(FA_Id, Start)),
  gensym(q, End),
  assert(nfa_delta(FA_Id, Start, RE, End)),
  assert(nfa_final(FA_Id, End)).

%-----------seq(a, b)----------
nfa_regexp_comp(FA_Id, RE) :-
  nonvar(FA_Id),
  is_regexp(RE),
  compound(RE),
  RE =.. [H | T],
  H = seq,
  length(T, X),
  write(X),
  gensym(q, Start),
  assert(nfa_initial(FA_Id, Start)),
  genstate(FA_Id, X, T, Start).


genstate(FA_Id, 0, [], Precdente):- 
  assert(nfa_final(FA_Id, Precdente)),
  write('assert final state').
genstate(FA_Id, X, [H| T], Precdente):-
  gensym(q, New_State),
  assert(nfa_delta(FA_Id, Precdente, H, New_State)),
  Y is X - 1,
  genstate(FA_Id, Y, T, New_State).




nfa_accept(FA_Id, [I | Is], S) :- nfa_delta(FA_Id, S, I, N), nfa_accept(FA_Id, Is, N).
nfa_accept(FA_Id, [], Q) :- nfa_final(FA_Id, Q).


nfa_test(FA_Id, Input):-
  nfa_initial(FA_Id, S), nfa_accept(FA_Id, Input, S).






%-------LIST--------
nfa_list(FA_Id):-
listing(nfa_initial(FA_Id, _)),
listing(nfa_final(FA_Id, _)),
listing(nfa_delta(FA_Id, _, _, _)).

