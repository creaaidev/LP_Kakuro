% Martim Rosa Monis ist199281 :pray: :pavaobless:.

% gets stuff provided by the teachers.
:- [codigo_comum].

%TADS for espaco
espaco_vars(espaco(_, Vars), Vars).
espaco_soma(espaco(Soma, _), Soma).

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
	sort(0, <, UPerms, Perms).

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
	append(Vars1, Vars2, VarsMix),
	not_is_set(VarsMix).

not_is_set(List) :-
	\+is_set(List).

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
	setof(Perm, (permutacoes_soma(Len, [1,2,3,4,5,6,7,8,9], Soma, Perm)), XPerms),
	append(XPerms, Perms),
	append(OPerms, [[espaco(Soma, Vars), Perms]], NPerms),
	permutacoes_soma_aux(R, NPerms, Perms_soma).

%trocar o findall por setof

%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
%Perms_soma e' resultado do permutacoes_soma_espacos
%olhar para as permutacoes_soma, findall faz mais do q eu penso normalmente
%so existe um outro espaco comum em que a variavel pode existir :)
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	espacos_com_posicoes_comuns(Espacos, Esp, Esps_coms),
	member(Espaco, Perms_soma),
	Espaco = [Esp, Perms],
	length(Esps_coms, Len),
	findall(OrigPerm, (member(OrigPerm, Perms), verifica_unificavel(OrigPerm, Esps_coms, Perms_soma, 0, Len)), Res),
	member(Perm, Res). 

verifica_unificavel(_, _, _, Len, Len).

verifica_unificavel(OrigPerm, Esps_coms, Perms_soma, Index, Len) :-
	black_magic(OrigPerm, Esps_coms, Perms_soma, Index),
	NewIndex is Index+1,
	verifica_unificavel(OrigPerm, Esps_coms, Perms_soma, NewIndex, Len).

black_magic(OrigPerm, Esps_coms, Perms_soma, Index) :-
	nth0(Index, Esps_coms, Esp1),
       	member(Esp2, Perms_soma),
       	Esp2 = [Esp1, PermsC],
       	append(PermsC, Perms),
	nth0(Index, OrigPerm, Value),
	is_in(Value, Perms).

selespaco([Espaco, _], Espaco).

selperms([], ResPerms, ResPerms).

selperms([[_, Perms] | R], Aux, ResPerms) :-
	append(Aux, Perms, NAux),
	selperms(R, NAux, ResPerms).

%permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
%
%permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
%espaco_vars(Esp, EspVars),
%findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms),
%append([EspVars], [Perms], Perms_poss), !.

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [EspVars, Perms]) :-
 	espaco_vars(Esp, EspVars),
	setof(Perm, Esp^permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms), !.

permutacoes_possiveis_espaco(_, _, _, []). %gaspa melita stuff

%permutacoes_possiveis_espacoS(Espacos, Perms_poss_esps)
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
	permutacoes_soma_espacos(Espacos, Perms_soma),
	bagof(Perm_poss, Espaco^(member(Espaco, Espacos), permutacoes_possiveis_espaco(Espacos, Perms_soma, Espaco, Perm_poss)), Perms_poss_esps).

%numeros_comuns(Lst_Perms, Numeros_comuns)
numeros_comuns(Lst_Perms, Numeros_comuns) :-
	nth1(1, Lst_Perms, Perm),
	length(Perm, LenPerm),
	num_comuns_aux(Lst_Perms, [], Numeros_comuns, 1, LenPerm), !.

num_comuns_aux(_, Numeros_comuns, Numeros_comuns, Index, LenPerm) :-
	Index =:= LenPerm+1, !.

num_comuns_aux(Lst_Perms, Aux, Numeros_comuns, Index, LenPerm) :-
	findall(Value, (member(X, Lst_Perms), nth1(Index, X, Value)), Values),
	list_to_set(Values, SetValue),
	num_comuns_aux2(SetValue, Aux, Index, NewAux),
	NewIndex is Index+1,
	num_comuns_aux(Lst_Perms, NewAux, Numeros_comuns, NewIndex, LenPerm).

num_comuns_aux2(SetValue, Aux, _, Aux) :-
	length(SetValue, SetLen),
	SetLen \== 1.

num_comuns_aux2([SetValue], Aux, Index, NewAux) :-
	append(Aux, [(Index, SetValue)], NewAux).

%atribui_comuns(Perms_Possiveis)
atribui_comuns([PermPoss | R]) :-
	unifica_comuns(PermPoss),
	atribui_comuns(R).

atribui_comuns([]).

unifica_comuns([EspVars, Perms]) :-
	numeros_comuns(Perms, Numeros_comuns),
	atribui_valor(Numeros_comuns, EspVars).

atribui_valor([], _).

atribui_valor([PV | R], EspVars) :-
	get_pos(PV, Pos),
	get_valor(PV, Valor),
	nth1(Pos, EspVars, Var),
	Var = Valor,
	atribui_valor(R, EspVars).

get_pos((Pos, _), Pos).
get_valor((_, Valor), Valor).

%retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
	retira_aux_1(Perms_Possiveis, [], Novas_Perms_Possiveis).

retira_aux_1([], Novas_Perms_Possiveis, Novas_Perms_Possiveis).

retira_aux_1([PermPoss | R], Aux, Novas_Perms_Possiveis) :-
	retira_aux(PermPoss, UPermPoss),
	append(Aux, [UPermPoss], NewAux),
	retira_aux_1(R, NewAux, Novas_Perms_Possiveis).

retira_aux([EspVars, Perms], UPermPoss) :-
	findall(Perm, (member(Perm, Perms), testa_unifi(Perm, EspVars)), UnifPerms),
	UPermPoss = [EspVars, UnifPerms].

testa_unifi(Perm, EspVars) :-
	\+ \+ Perm = EspVars.

%simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Aux),
	Aux \== Perms_Possiveis,
	simplifica(Aux, Novas_Perms_Possiveis), !.

simplifica(Perms_Possiveis, Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Aux),
	Aux == Perms_Possiveis.

%inicializa(Puzzle, Perms_Possiveis)
inicializa(Puzzle, Perms_Possiveis) :-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Old_Perms_Possiveis),
	simplifica(Old_Perms_Possiveis, Perms_Possiveis), !.

%escolhe a permutacao possivel com o menor numero de alternativas, primeira que apareca
%escolhe_menos_alternativas(Perms_Possiveis, Escolha)
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
	include(verifica, Perms_Possiveis, Verificados),
	Verificados \== [],
	maplist(permlen, Verificados, Comprimentos),  
	min_list(Comprimentos, Menor),
	nth0(FirstMenor, Comprimentos, Menor), !,
	nth0(FirstMenor, Verificados, Escolha).

permlen(X, Len) :-
	get_perms(X, Perms),
	length(Perms, Len).

verifica(Esp_e_Perms) :-
	get_perms(Esp_e_Perms, Perms),
	length(Perms, Len),
	Len > 1.

get_perms([_, Perms], Perms).

%experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
	Escolha = [EspVars, Perms], 
	member(Perm, Perms),
	EspVars = Perm,
	replace(Escolha, [EspVars, [Perm]], Perms_Possiveis, Novas_Perms_Possiveis).

replace(_, _, [], []).
replace(O, R, [H|T], [R|T2]) :- H == O, replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \== O, replace(O, R, T, T2).

%resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
	escolhe_menos_alternativas(Perms_Possiveis, Escolha), !, 
	experimenta_perm(Escolha, Perms_Possiveis, NPermsExperimenta), 
	simplifica(NPermsExperimenta, Novas_Perms_Aux),
	resolve_aux(Novas_Perms_Aux, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
	simplifica(Perms_Possiveis, Novas_Perms_Possiveis).

%resolve(Puz)
resolve(Puz) :-
	inicializa(Puz, Perms_Possiveis),
	resolve_aux(Perms_Possiveis, _).
