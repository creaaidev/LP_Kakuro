% Martim Rosa Monis ist199281 :pray: :pavaobless:.

% ?/20 Completeness Ratio

% gets stuff provided by the teachers.
:- [codigo_comum, puzzles_publicos].

% gets the sum of the elements in a given list.
soma_lista([], 0).
soma_lista([P | R], Res) :- 
	soma_lista(R, NRes), Res is P + NRes.
%existe sum_list built-in

% returns a list with N elements of Els that have sum equal to N.
comb_soma_valida(N, Els, Soma, L) :- 
	combinacao(N, Els, L),
	soma_lista(L, NSoma),
	NSoma =:= Soma.

% returns a sorted list with all the possible combinations of N numbers that have sum equal to N.
combinacoes_soma(N, Els, Soma, Combs) :-
	bagof(Comb, comb_soma_valida(N, Els, Soma, Comb), Combs).

% returns a sorted list with all possible permutations (...), also gaspa is love, gaspa is life 
permutacoes_soma(N, Els, Soma, Perms) :-
	combinacoes_soma(N, Els, Soma, Combs),
	findall(Perm, (member(X, Combs), permutation(X, Perm)), UPerms),
	sort(1, @<=, UPerms, Perms).

% eu meti um = a seguir ao @<, verificar mais tarde

% tou a 3+ horas nisto WOOOO WOOOO WOOOO
% again tysm gaspa you are a real lifesaver, also trace helped a lot here
% espaco_fila(Fila, Esp, H_V) , espaco(Soma, [variaveis])
get_space(Soma, Vars, [espaco(Soma, Vars)]) :-
	Vars \= [].

get_space(_, Vars, []) :-
	Vars == [].

get_sum(v, [V, _], V).
get_sum(h, [_, H], H).

espaco_fila(Fila, Esp, H_V) :-
	espaco_fila_aux(Fila, Esp, H_V, 0, [], []).

espaco_fila_aux([P | R], Esp, H_V, Soma, Vars, Esps) :- 
	is_list(P),			%P is a list
	get_space(Soma, Vars, Space), 	%Makes the last space
	append(Esps, Space, NEsps),	%Appends the last space
	get_sum(H_V, P, NSoma), 	%Gets the new sum
	espaco_fila_aux(R, Esp, H_V, NSoma, [], NEsps).	%Resets Vars

espaco_fila_aux([P | R], Esp, H_V, Soma, Vars, Esps) :-
	var(P),				%P is a Var
	VarAux = Vars,
	append(VarAux, [P], NVars),
	espaco_fila_aux(R, Esp, H_V, Soma, NVars, Esps).

espaco_fila_aux([], Esp, _, Soma, Vars, Esps) :-
	get_space(Soma, Vars, Space),   %Makes the last space
	append(Esps, Space, NEsps),	%Appends the last space
	member(Esp, NEsps).		%Gets a member from Esps

%espacos_fila(H_V, Fila, Espacos)
%Gets all spaces from a Fila and saves them in Esps
espacos_fila(H_V, Fila, Esps) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Esps).

%Thank You Pedro Quintas :)
espacos_fila(H_V, Fila, []) :-
	\+bagof(Esp, espaco_fila(Fila, Esp, H_V), _).

%espacos_puzzle(Puzzle, Espacos)
%Gets all the spaces from a puzzle
espacos_puzzle(Puzzle, Esps) :-
	espacos_puzzle_aux(Puzzle, h, [], Espsh),
	mat_transposta(Puzzle, PTrans),
	espacos_puzzle_aux(PTrans, v, [],Espsv),
	append(Espsh, Espsv, Esps).

espacos_puzzle_aux([], _, Esps, Esps).

espacos_puzzle_aux([L | R], H_V, OEsps, Esps) :-
	espacos_fila(H_V, L, CEsps),
	append(OEsps, CEsps, NEsps),
	espacos_puzzle_aux(R, H_V, NEsps, Esps).

%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
%Order shouldn't be changed, Esps_com is a list with all the spaces that have shared variables with Esp
%Thank You Joao for helping me with this one, unifying is a real pain in the ass
espacos_com_posicoes_comuns(Esps, Esp, Esps_com) :-
	bagof(E, (member(E, Esps), E \== Esp, partilha(E, Esp)), Esps_com).

espacos_com_posicoes_comuns(Esps, Esp, Esps_com) :-
	\+bagof(E, (member(E, Esps), E \== Esp, partilha(E, Esp)), Esps_com).

%Checks whether or not E shares variables with Esp
partilha(espaco(_, Vars1), espaco(_, Vars2)) :-
	intersecao(Vars1, Vars2, Vars),
	length(Vars, Len),
	Len > 0.

intersecao([], _, []) :- !.

intersecao([P | R], Vars2, [P | Vars]) :-
	is_in(P, Vars2), !,
	intersecao(R, Vars2, Vars).

intersecao([P | R], Vars2, Vars) :-
	not_is_in(P, Vars2),
	intersecao(R, Vars2, Vars).

not_is_in(P, Vars) :-
	\+is_in(P, Vars).

is_in(_, []) :- fail, !.
is_in(P, [F | _]) :- P == F, !.
is_in(P, [F | R]) :- P \== F, is_in(P, R).

%permutacoes_soma_espacos(Espacos, Perms_soma)
%Perms_soma is a list of lists with 2 elements, first being a space from Espacos and second a list
% of permutations that have sum equal to sum of space
permutacoes_soma_espacos(Espacos, Perms_soma) :-
	permutacoes_soma_aux(Espacos, [], Perms_soma).

permutacoes_soma_aux([], Perms_soma, Perms_soma).

permutacoes_soma_aux([espaco(Soma, Vars) | R], OPerms, Perms_soma) :-
	length(Vars, Len),
	bagof(Perm, (permutacoes_soma(Len, [1,2,3,4,5,6,7,8,9], Soma, Perm)), XPerms),
	append(XPerms, XXPerms),
	sort(1, @=<, XXPerms, Perms),
	append(OPerms, [[espaco(Soma, Vars), Perms]], NPerms),
	permutacoes_soma_aux(R, NPerms, Perms_soma).

%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
%Perms_soma e' resultado do permutacoes_soma_espacos
%olhar para as permutacoes_soma, findall faz mais do q eu penso normalmente
%o afonso usou 3 e 1 .->
%meter TADs no fim
