% Martim Rosa Monis ist199281 :pray: :pavaobless:

% gets stuff provided by the teachers
:- [codigo_comum, puzzles_publicos].

soma_lista([], 0).
soma_lista([P | R], Res) :- soma_lista(R, NRes), Res is P + NRes.

comb_soma_valida(N, Els, Soma, L) :- combinacao(N, Els, L), soma_lista(L, NSoma), NSoma =:= Soma.

combinacoes_soma(N, Els, Soma, Combs) :- bagof(Comb, comb_soma_valida(N, Els, Soma, Comb), Combs), !.
