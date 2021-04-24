% Martim Rosa Monis ist199281 :pray: :pavaobless:.

% 2/20 Completeness Ratio

% gets stuff provided by the teachers.
:- [codigo_comum, puzzles_publicos].

% gets the sum of the elements in a given list.
soma_lista([], 0).
soma_lista([P | R], Res) :- soma_lista(R, NRes), Res is P + NRes.

% returns a list with N elements of Els that have sum equal to N.
comb_soma_valida(N, Els, Soma, L) :- combinacao(N, Els, L), soma_lista(L, NSoma), NSoma =:= Soma.

% returns a sorted list with all the possible combinations of N numbers that have sum equal to N.
combinacoes_soma(N, Els, Soma, Combs) :- bagof(Comb, comb_soma_valida(N, Els, Soma, Comb), Combs).

% returns a sorted list with all possible permutations (...), also gaspa is love, gaspa is life 
permutacoes_soma(N, Els, Soma, Perms) :- combinacoes_soma(N, Els, Soma, Combs), findall(Perm, (member(X, Combs), permutation(X, Perm)), UPerms), sort(1, @<, UPerms, Perms).

% espaco_fila(Fila, Esp, H_V) , espaco(Soma, [variaveis]), unfinished mess cena para dizer a soma, cena para guardar vars

espaco_fila(Fila, Esp, H_V) :- esp_fila_aux(Fila, Aux, [], [], H_V)
esp_fila_aux([], Soma, Vars, Dir) :- 
esp_fila_aux([[V | _] | R], V, Vars, v) :- esp_fila_aux(R, V, Vars, v).
esp_fila_aux([[_ | H] | R], V, Vars, h) :- esp_fila_aux(R, H, Vars, h).
esp_fila_aux([P | R], Soma, [P | ReV], Dir) :- esp_fila_aux(R, Soma, ReV, Dir)
