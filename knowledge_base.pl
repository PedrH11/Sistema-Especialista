:- encoding(utf8).
% =========================================
% BASE DE CONHECIMENTO PONTUADA
% Formato: sintoma(Doenca, Sintoma, Peso).
% =========================================

% --- Gripe (Sintomas genéricos) ---
sintoma(gripe, febre, 5).
sintoma(gripe, tosse, 10).
sintoma(gripe, dor_no_corpo, 8).
sintoma(gripe, cansaco, 5).
sintoma(gripe, dor_de_cabeca, 4).

% --- Resfriado (Sintomas leves) ---
sintoma(resfriado, espirros, 10).
sintoma(resfriado, coriza, 10).
sintoma(resfriado, dor_de_garganta, 8).
sintoma(resfriado, tosse_leve, 5).

% --- Covid (Sintomas específicos ganham mais pontos) ---
sintoma(covid, febre, 5).
sintoma(covid, tosse_seca, 8).
sintoma(covid, falta_de_ar, 15).
sintoma(covid, perda_olfato, 20). 
sintoma(covid, perda_paladar, 20).
sintoma(covid, cansaco, 5).

% --- Pneumonia (Grave - Ligado a Idoso/Fumante) ---
sintoma(pneumonia, febre_alta, 5).
sintoma(pneumonia, tosse, 5).
sintoma(pneumonia, falta_de_ar, 15).
sintoma(pneumonia, dor_no_peito, 15).
sintoma(pneumonia, cansaco, 5).

% --- Sinusite ---
sintoma(sinusite, dor_de_cabeca, 5).
sintoma(sinusite, congestao_nasal, 10).
sintoma(sinusite, dor_na_face, 15). 
sintoma(sinusite, coriza, 5).

% --- Dengue ---
sintoma(dengue, febre_alta, 10).
sintoma(dengue, dor_atras_dos_olhos, 15). 
sintoma(dengue, manchas_na_pele, 10).
sintoma(dengue, dor_nas_juntas, 8).
sintoma(dengue, dor_de_cabeca, 4).

% --- Zika ---
sintoma(zika, manchas_na_pele, 12).
sintoma(zika, coceira, 15). 
sintoma(zika, olhos_vermelhos, 12).
sintoma(zika, febre_baixa, 8).

% --- Chikungunya ---
sintoma(chikungunya, febre_alta, 10).
sintoma(chikungunya, dor_nas_juntas, 20). % Dor muito forte
sintoma(chikungunya, inchaco_nas_juntas, 15).
sintoma(chikungunya, manchas_na_pele, 5).

% --- Intoxicação Alimentar (Ligado a Comida Suspeita) ---
sintoma(intoxicacao_alimentar, nausea, 10).
sintoma(intoxicacao_alimentar, vomito, 15).
sintoma(intoxicacao_alimentar, dor_abdominal, 15).
sintoma(intoxicacao_alimentar, diarreia, 15).
sintoma(intoxicacao_alimentar, febre, 5).

% --- Enxaqueca (Ligado a Estresse) ---
sintoma(enxaqueca, dor_de_cabeca, 5).
sintoma(enxaqueca, dor_de_cabeca_pulsante, 20).
sintoma(enxaqueca, sensibilidade_luz, 15).
sintoma(enxaqueca, nausea, 8).

% --- Gastrite (Ligado a Estresse) ---
sintoma(gastrite, dor_no_estomago, 15).
sintoma(gastrite, queimacao, 15).
sintoma(gastrite, nausea, 5).
sintoma(gastrite, perda_de_apetite, 5).


% =========================================
% RECOMENDAÇÕES E CONSELHOS MÉDICOS
% =========================================

conselho(gripe, "Repouso, hidratação constante e uso de antitérmicos se houver febre alta.").
conselho(resfriado, "Lavagem nasal com soro fisiológico e hidratação.").
conselho(covid, "Isolamento social imediato. Monitore a saturação de oxigênio e febre.").
conselho(pneumonia, "URGENTE: Procurar atendimento médico para Raio-X e possível uso de antibióticos.").
conselho(sinusite, "Lavagem nasal abundante, inalação e compressas mornas no rosto.").
conselho(rinite, "Evitar contato com poeira, ácaros e pelos de animais. Lavar o nariz.").

% --- Alertas Importantes ---
conselho(dengue, "ALERTA: Hidratação agressiva é vital. PROIBIDO usar AAS/Aspirina pelo risco de hemorragia.").
conselho(zika, "Repouso e uso de repelente para evitar transmitir a doença a outras pessoas.").
conselho(chikungunya, "Repouso absoluto e compressas frias nas articulações para aliviar a dor.").

conselho(intoxicacao_alimentar, "Soro caseiro para reidratação, dieta leve e evitar laticínios.").
conselho(enxaqueca, "Descanso em local escuro e silencioso. Evitar telas e luzes fortes.").
conselho(gastrite, "Evitar longos períodos em jejum, café, pimenta e alimentos ácidos.").


% =========================================
% REGRAS
% =========================================

% 1. Listar TODOS os sintomas únicos
todos_sintomas(Lista) :-
    setof(S, D^P^sintoma(D, S, P), Lista).

% 2. Regra Base (Diagnóstico por Sintomas)
diagnostico_pontuado(Doenca, ListaSintomasUsuario, PontuacaoTotal) :-
    distinct(Doenca, sintoma(Doenca, _, _)),
    findall(P, (sintoma(Doenca, S, P), member(S, ListaSintomasUsuario)), ListaPesos),
    sum_list(ListaPesos, PontuacaoTotal),
    PontuacaoTotal > 0.


% =========================================
% FATORES DE RISCO (CONTEXTO)
% =========================================

% Fator: Idoso
fator_risco(covid, idoso, 15).
fator_risco(gripe, idoso, 10).
fator_risco(pneumonia, idoso, 20). % Risco alto!

% Fator: Fumante
fator_risco(sinusite, fumante, 15).
fator_risco(pneumonia, fumante, 20).
fator_risco(covid, fumante, 5).

% Fator: Gestante
fator_risco(zika, gestante, 30).
fator_risco(dengue, gestante, 10).

% Fator: Exposição a Mosquitos
fator_risco(dengue, area_mosquitos, 15).
fator_risco(zika, area_mosquitos, 15).
fator_risco(chikungunya, area_mosquitos, 15).

% Fator: Comida Suspeita (Resolve Intoxicação)
fator_risco(intoxicacao_alimentar, comida_suspeita, 30).

% Fator: Estresse (Resolve Enxaqueca e Gastrite)
fator_risco(enxaqueca, estresse_alto, 20).
fator_risco(gastrite, estresse_alto, 15).


% --- NOVA REGRA DE DIAGNÓSTICO COMPLETO ---
% Soma: Pontos dos Sintomas + Pontos dos Fatores de Risco
diagnostico_completo(Doenca, ListaSintomas, ListaFatores, PontosFinais) :-
    diagnostico_pontuado(Doenca, ListaSintomas, PontosSintomas),
    findall(P, (member(Fator, ListaFatores), fator_risco(Doenca, Fator, P)), ListaPesosFatores),
    sum_list(ListaPesosFatores, PontosFatores),
    PontosFinais is PontosSintomas + PontosFatores.

    % =========================================
    
% CLASSIFICAÇÃO DE GRAVIDADE (TRIAGEM)
% Niveis: emergenc, atencao, rotina
% =========================================

gravidade(pneumonia, emergencia).
gravidade(intoxicacao_alimentar, emergencia). 

gravidade(covid, atencao).
gravidade(dengue, atencao).
gravidade(zika, atencao).
gravidade(chikungunya, atencao).
gravidade(gastrite, atencao).

gravidade(gripe, rotina).
gravidade(resfriado, rotina).
gravidade(sinusite, rotina).
gravidade(rinite, rotina).
gravidade(enxaqueca, rotina).