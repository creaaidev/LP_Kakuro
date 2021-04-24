% Martim Rosa Monis ist199281 :pray: :pavaobless:.

% gets stuff provided by the teachers.
:- [codigo_comum, puzzles_publicos].

% gets the sum of the elements in a given list.
soma_lista([], 0).
soma_lista([P | R], Res) :- soma_lista(R, NRes), Res is P + NRes.

% returns a list with N elements of Els that have sum equal to N.
comb_soma_valida(N, Els, Soma, L) :- combinacao(N, Els, L), soma_lista(L, NSoma), NSoma =:= Soma.

% returns a sorted list with all the possible combinations of N numbers that have sum equal to N.
combinacoes_soma(N, Els, Soma, Combs) :- bagof(Comb, comb_soma_valida(N, Els, Soma, Comb), Combs), !.

permutacoes_soma(N, Els, Soma, Perms)
