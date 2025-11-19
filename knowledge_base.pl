:- encoding(utf8).
% =========================================
% BASE DE CONHECIMENTO PONTUADA
% Formato: sintoma(Doenca, Sintoma, Peso).
% =========================================

% --- Gripe (Sintomas genéricos) ---
sintoma(gripe, febre, 5).
sintoma(gripe, tosse, 5).
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

% =========================================
% RECOMENDAÇÕES E CONSELHOS MÉDICOS
% =========================================

conselho(gripe, "Repouso, hidratação constante e uso de antitérmicos se houver febre alta.").
conselho(resfriado, "Lavagem nasal com soro fisiológico e hidratação.").
conselho(covid, "Isolamento social imediato. Monitore a saturação de oxigênio e febre.").
conselho(sinusite, "Lavagem nasal abundante, inalação e compressas mornas no rosto.").
conselho(rinite, "Evitar contato com poeira, ácaros e pelos de animais. Lavar o nariz.").

% --- Alertas Importantes ---
conselho(dengue, "ALERTA: Hidratação agressiva é vital. PROIBIDO usar AAS/Aspirina pelo risco de hemorragia.").
conselho(zika, "Repouso e uso de repelente para evitar transmitir a doença a outras pessoas.").
conselho(chikungunya, "Repouso absoluto e compressas frias nas articulações para aliviar a dor.").

conselho(intoxicacao_alimentar, "Soro caseiro, dieta leve e evitar laticínios temporariamente.").
conselho(enxaqueca, "Descanso em local escuro e silencioso. Evitar telas e luzes fortes.").

% =========================================
% REGRAS
% =========================================

% 1. Listar TODOS os sintomas únicos (ignora o peso '_')
todos_sintomas(Lista) :-
    setof(S, D^P^sintoma(D, S, P), Lista).

% 2. Regra de Diagnóstico com Pontuação
%    Para uma Doença dada e uma Lista de Sintomas do Usuário,
%    calculamos o total de pontos somando os pesos dos sintomas coincidentes.

diagnostico_pontuado(Doenca, ListaSintomasUsuario, PontuacaoTotal) :-
    % Primeiro, garantimos que a doença existe na base
    distinct(Doenca, sintoma(Doenca, _, _)),
    
    % Encontramos todos os pesos (P) onde:
    % - A Doença tem o sintoma S com peso P
    % - E o sintoma S está na lista do usuário
    findall(P, (sintoma(Doenca, S, P), member(S, ListaSintomasUsuario)), ListaPesos),
    
    % Somamos os pesos
    sum_list(ListaPesos, PontuacaoTotal),
    
    % Só retornamos doenças que tenham pontuação maior que 0
    PontuacaoTotal > 0.