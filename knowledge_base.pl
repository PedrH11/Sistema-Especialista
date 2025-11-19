% Fatos: Sintomas de doenças
sintoma(gripe, febre).
sintoma(gripe, tosse).
sintoma(gripe, dor_cabeca).

sintoma(dengue, febre).
sintoma(dengue, manchas).
sintoma(dengue, dor_cabeca).

sintoma(covid, febre).
sintoma(covid, tosse).
sintoma(covid, perda_olfato).

sintoma(pedra_no_rim, colica_renal).
sintoma(pedra_no_rim, dor_nas_costas).
sintoma(pedra_no_rim, sangue_na_urina).

sintoma(gastrite, dor_abdominal).
sintoma(gastrite, azia).
sintoma(gastrite, nausea).

% Regra: Um diagnóstico é X se o paciente tem Sintoma1 E Sintoma2
% Nota: S1 \= S2 garante que não usamos o mesmo sintoma duas vezes.
diagnostico(Doenca, S1, S2) :-
    sintoma(Doenca, S1),
    sintoma(Doenca, S2),
    S1 \= S2.