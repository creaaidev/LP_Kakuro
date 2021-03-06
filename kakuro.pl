% Martim Rosa Monis ist199281 :pray: :pavaobless: :gaspabless: :afonsobless:

% gets stuff provided by the teachers.
:- [codigo_comum].

%TADS for espaco
espaco_vars(espaco(_, Vars), Vars).
espaco_soma(espaco(Soma, _), Soma).

not_is_in(P, Vars) :-
	\+is_in(P, Vars).

is_in(_, []) :- fail, !.
is_in(P, [F | _]) :- P == F, !.
is_in(P, [F | R]) :- P \== F, is_in(P, R).

% returns a list with N elements of Els that have sum equal to N
comb_soma_valida(N, Els, Soma, L) :- 
	combinacao(N, Els, L),
	sum_list(L, NSoma),
	NSoma =:= Soma.

% Sorted list with all possible combinations of N numbers which sum equal to N.
combinacoes_soma(N, Els, Soma, Combs) :-
	bagof(Comb, comb_soma_valida(N, Els, Soma, Comb), Combs).

% Sorted list with all possible permutations which sum is N
permutacoes_soma(N, Els, Soma, Perms) :-
	combinacoes_soma(N, Els, Soma, Combs),
	findall(Perm, (member(X, Combs), permutation(X, Perm)), UPerms),
	sort(0, <, UPerms, Perms).

% espaco_fila(Fila, Esp, H_V) , espaco(Soma, [variaveis])
% Gets all the spaces from a given list
get_space(Soma, Vars, [espaco(Soma, Vars)]) :-
	Vars \= [].

get_space(_, Vars, []) :-
	Vars == [].

get_sum(v, [V, _], V). % if checking vertical space
get_sum(h, [_, H], H). % if checking horizontal space

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
	append(VarAux, [P], NVars), %Add vars to Var list
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
	espacos_puzzle_aux(Puzzle, h, [], Espsh), %Gets all the horizontal spaces
	mat_transposta(Puzzle, PTrans), %Transposes the matrix
	espacos_puzzle_aux(PTrans, v, [],Espsv), %Gets all the vertical spaces
	append(Espsh, Espsv, Esps). %Joins the horizontal spcs with vertical spaces

espacos_puzzle_aux([], _, Esps, Esps).

espacos_puzzle_aux([L | R], H_V, OEsps, Esps) :-
	espacos_fila(H_V, L, CEsps), %Gets the spaces of the current line/column
	append(OEsps, CEsps, NEsps),
	espacos_puzzle_aux(R, H_V, NEsps, Esps). %Go to next line/column

%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
%Esps_com is a list with all the spaces that have shared variables with Esp
espacos_com_posicoes_comuns(Esps, Esp, Esps_com) :-
	bagof(E, (member(E, Esps), E \== Esp, partilha(E, Esp)), Esps_com).

espacos_com_posicoes_comuns(Esps, Esp, Esps_com) :-
	\+bagof(E, (member(E, Esps), E \== Esp, partilha(E, Esp)), Esps_com).

%Checks whether or not E shares variables with Esp
partilha(espaco(_, Vars1), espaco(_, Vars2)) :-
	append(Vars1, Vars2, VarsMix), %Puts all the variables into a mix
	not_is_set(VarsMix). %theres repeated vars(not a set), then shares vars

not_is_set(List) :-
	\+is_set(List).

%permutacoes_soma_espacos(Espacos, Perms_soma)
%Perms_soma is a list of lists with 2 elements, first being a space from
% of permutations that have sum equal to sum of space Espacos and second a list
permutacoes_soma_espacos(Espacos, Perms_soma) :-
	permutacoes_soma_aux(Espacos, [], Perms_soma). %Starts Aux empty

permutacoes_soma_aux([], Perms_soma, Perms_soma). %End scenario

permutacoes_soma_aux([espaco(Soma, Vars) | R], OPerms, Perms_soma) :-
	length(Vars, Len), %Gets the length of Vars
	setof(Perm, (permutacoes_soma(Len,[1,2,3,4,5,6,7,8,9],Soma,Perm)), XPerms),
	append(XPerms, Perms), %Gets the Perms with Len and Soma
	append(OPerms,[[espaco(Soma, Vars), Perms]],NPerms),%Add SpaceWPerms to Aux
	permutacoes_soma_aux(R, NPerms, Perms_soma).

%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
%Gets a possible permutation for a given space
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	espacos_com_posicoes_comuns(Espacos, Esp, Esps_coms), %Gets espacos comuns
	member(Espaco, Perms_soma), %Get space from Perms_Soma thats in Esps_coms,
	Espaco = [Esp, Perms], % thus getting its Permutations as well
	length(Esps_coms, Len), %Gets the number of espacos comuns
	findall(OrigPerm, (member(OrigPerm, Perms),
       	verifica_unificavel(OrigPerm, Esps_coms, Perms_soma, 0, Len)), Res),
	member(Perm, Res).

verifica_unificavel(_, _, _, Len, Len).

verifica_unificavel(OrigPerm, Esps_coms, Perms_soma, Index, Len) :-
	black_magic(OrigPerm, Esps_coms, Perms_soma, Index), %Checks whether or not
	NewIndex is Index+1, % the value for current var is in the current Perm
	verifica_unificavel(OrigPerm, Esps_coms, Perms_soma, NewIndex, Len).

black_magic(OrigPerm, Esps_coms, Perms_soma, Index) :-
	nth0(Index, Esps_coms, Esp1), %nth permutation value is in nth espaco comum
       	member(Esp2, Perms_soma), %gets the perms of the space
       	Esp2 = [Esp1, PermsC],
       	append(PermsC, Perms), %makes it a flat list
	nth0(Index, OrigPerm, Value), %Gets the nth value of the original perm
	is_in(Value, Perms). %Checks whether or not value is in the mix of values

selespaco([Espaco, _], Espaco). %Gets space when the format is [Espaco, Perms]

%permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
%Gets all the possible permutations for a given space
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [EspVars, Perms]) :-
 	espaco_vars(Esp, EspVars),
	setof(Perm, Esp^permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma),
	 Perms), !.

permutacoes_possiveis_espaco(_, _, _, []).

%permutacoes_possiveis_espacoS(Espacos, Perms_poss_esps)
%Gets all the possible permutations for a group of spaces
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
	permutacoes_soma_espacos(Espacos, Perms_soma),
	bagof(Perm_poss, Espaco^(member(Espaco, Espacos),
       	permutacoes_possiveis_espaco(Espacos, Perms_soma, Espaco, Perm_poss)),
		    Perms_poss_esps).

%numeros_comuns(Lst_Perms, Numeros_comuns)
%Gets common numbers from a list of permutations, with their pos and value
% like so, (pos, value)
numeros_comuns(Lst_Perms, Numeros_comuns) :-
	nth1(1, Lst_Perms, Perm), %Get the len of first Perm
	length(Perm, LenPerm), %To know how many numbers in a Perm there are
	num_comuns_aux(Lst_Perms, [], Numeros_comuns, 1, LenPerm), !.

num_comuns_aux(_, Numeros_comuns, Numeros_comuns, Index, LenPerm) :-
	Index > LenPerm, !. %If checked all values(Index > LenPerm), then stop

num_comuns_aux(Lst_Perms, Aux, Numeros_comuns, Index, LenPerm) :-
	%Gets all values in index
	findall(Value, (member(X, Lst_Perms), nth1(Index, X, Value)), Values),
	list_to_set(Values, SetValue), %Remove repeated values
	num_comuns_aux2(SetValue, Aux, Index, NewAux),
	NewIndex is Index+1,
	num_comuns_aux(Lst_Perms, NewAux, Numeros_comuns, NewIndex, LenPerm).

num_comuns_aux2(SetValue, Aux, _, Aux) :-
	length(SetValue, SetLen),
	SetLen \== 1. %If more than one value, then its not common value

num_comuns_aux2([SetValue], Aux, Index, NewAux) :-
	append(Aux, [(Index, SetValue)], NewAux). %All perms have the value
	% in given index, so save it

%atribui_comuns(Perms_Possiveis)
%Assigns/unifies all common values
atribui_comuns([PermPoss | R]) :-
	unifica_comuns(PermPoss), %for a given Space and Perms
	atribui_comuns(R).

atribui_comuns([]).

unifica_comuns([EspVars, Perms]) :-
	numeros_comuns(Perms, Numeros_comuns), %Get common numbers
	atribui_valor(Numeros_comuns, EspVars). 

atribui_valor([], _). %When all common values have been unified/assigned

atribui_valor([PV | R], EspVars) :- 
	get_pos(PV, Pos), %Get the pos in PV
	get_valor(PV, Valor), %Get the Value in PV
	nth1(Pos, EspVars, Var), %Unify the posnth Var with Value
	Var = Valor, %(Continuation of above)
	atribui_valor(R, EspVars). %Proceed to next common value

get_pos((Pos, _), Pos). %Get the Pos in a common value
get_valor((_, Valor), Valor). %Get the Value in a common value

%retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
%Removes incompatible/impossible permutations
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
	retira_aux_1(Perms_Possiveis, [], Novas_Perms_Possiveis).

retira_aux_1([], Novas_Perms_Possiveis, Novas_Perms_Possiveis).

retira_aux_1([PermPoss | R], Aux, Novas_Perms_Possiveis) :-
	retira_aux(PermPoss, UPermPoss), %gets all the perms that can unify
	append(Aux, [UPermPoss], NewAux), %with the Vars in PermPoss, add it to Aux
	retira_aux_1(R, NewAux, Novas_Perms_Possiveis). %proceed to next PermPoss

retira_aux([EspVars, Perms], UPermPoss) :-
	findall(Perm, (member(Perm, Perms), testa_unifi(Perm, EspVars)), UnifPerms),
	UPermPoss = [EspVars, UnifPerms].

testa_unifi(Perm, EspVars) :-
	\+ \+ Perm = EspVars. %Checks if Perm can unify with EspVars

%simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
%atribui, retira, until no further changes happen
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Aux),
	Aux \== Perms_Possiveis, %Changes were made, repeat the process
	simplifica(Aux, Novas_Perms_Possiveis), !.

simplifica(Perms_Possiveis, Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Aux),
	Aux == Perms_Possiveis. %No changes were made, stop the cycle

%inicializa(Puzzle, Perms_Possiveis)
inicializa(Puzzle, Perms_Possiveis) :-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Old_Perms_Possiveis),
	simplifica(Old_Perms_Possiveis, Perms_Possiveis), !.

%escolhe_menos_alternativas(Perms_Possiveis, Escolha)
%Chooses the first possible Esp&Perms with the least number of permutations
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
	include(verifica, Perms_Possiveis, Verificados), %Esp&Perms w/ > 1 Perm
	Verificados \== [], %At least one Esp&Perms with more than 1 Perm
	maplist(permlen, Verificados, CompPerms), %Get their lengths
	min_list(Comprimentos, Menor), %Find the smallest length
	nth0(FirstMenor, CompPerms, Menor), !, %First smallest length index
	nth0(FirstMenor, Verificados, Escolha). %Indexnth Esp&Perms from Verificados

%Get length of Perms of X
permlen(X, Len) :-
	get_perms(X, Perms),
	length(Perms, Len).

%Checks whether or not Esp_e_Perms is higher than 1
verifica(Esp_e_Perms) :-
	get_perms(Esp_e_Perms, Perms),
	length(Perms, Len),
	Len > 1.

get_perms([_, Perms], Perms).

%experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
%Tries a Perm from Perms of Escolha
experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
	Escolha = [EspVars, Perms],
	member(Perm, Perms), %Gets a Perm from Perms of Escolha
	EspVars = Perm, %Unify Vars with Perm
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

%resolve(Puz) em que Puz e um puzzle
resolve(Puz) :-
	inicializa(Puz, Perms_Possiveis),
	resolve_aux(Perms_Possiveis, _).
