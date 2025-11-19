# core/views.py
from django.shortcuts import render
from pyswip import Prolog
from django.conf import settings  # Para aceder ao settings.py
import os  # Para construir caminhos de ficheiros

# --- PASSO A: Inicialização do Motor Prolog ---

# 1. Definir o caminho para a nossa base de conhecimento
#    settings.BASE_DIR aponta para a raiz (meu_sistema_especialista/)
KB_PATH = os.path.join(settings.BASE_DIR, 'knowledge_base.pl')

# 2. (IMPORTANTE) Carregar o motor Prolog UMA VEZ.
#    Fazemos isto aqui fora da view (globalmente) por performance.
#    Assim, o Prolog não é recarregado a cada clique do utilizador.
prolog_engine = Prolog()
try:
    prolog_engine.consult(KB_PATH)
    print(f"Base de Conhecimento '{KB_PATH}' carregada com sucesso.")
except Exception as e:
    print(f"ERRO CRÍTICO: Não foi possível carregar '{KB_PATH}'. {e}")
    # Se falhar, o motor não funcionará
    prolog_engine = None 

# -----------------------------------------------

def consulta_view(request):
    context = {
        'resultado': None, # Agora será uma lista de dicionários: [{'nome': 'Gripe', 'pontos': 15}, ...]
        'erro': None,
        'sintomas_db': [],
        'sintomas_selecionados': []
    }

    if not prolog_engine:
        context['erro'] = "Motor Prolog não inicializado."
        return render(request, 'index.html', context)

    # --- PARTE 1: Obter a lista de TODOS os sintomas ---
    try:
        consulta_sintomas = list(prolog_engine.query("todos_sintomas(Lista)"))
        
        if consulta_sintomas:
            lista_suja = consulta_sintomas[0]['Lista']
            
            # --- MELHORIA AQUI ---
            # Vamos criar uma lista de dicionários/tuplas para facilitar no template.
            # Formato: [ ('valor_prolog', 'Texto Bonito'), ... ]
            sintomas_limpos = []
            for s in lista_suja:
                s_str = str(s)
                # Substitui underline por espaço e coloca Iniciais Maiúsculas
                texto_legivel = s_str.replace('_', ' ').title() 
                sintomas_limpos.append({'valor': s_str, 'texto': texto_legivel})
            
            # Ordenar alfabeticamente para facilitar a busca visual
            sintomas_limpos.sort(key=lambda x: x['texto'])
            
            context['sintomas_db'] = sintomas_limpos
            
    except Exception as e:
        context['erro'] = f"Erro ao carregar sintomas: {e}"

    # --- PARTE 2: Processar Diagnóstico ---
    if request.method == 'POST':
        selecionados = request.POST.getlist('sintomas')
        context['sintomas_selecionados'] = selecionados

        if not selecionados:
             context['erro'] = "Selecione ao menos um sintoma."
        else:
            try:
                # Formatar lista para o Prolog
                lista_prolog_str = "[" + ",".join(selecionados) + "]"
                
                # NOVA QUERY: Pede também a variável 'Pontos'
                query = f"diagnostico_pontuado(Doenca, {lista_prolog_str}, Pontos)"
                
                solucoes = list(prolog_engine.query(query))
                
                # Processar e ORDENAR os resultados
                resultados_formatados = []
                for sol in solucoes:
                    resultados_formatados.append({
                        'nome': sol['Doenca'].capitalize(),
                        'pontos': sol['Pontos']
                    })
                
                # Ordenar: Quem tem mais pontos aparece primeiro (reverse=True)
                resultados_formatados.sort(key=lambda x: x['pontos'], reverse=True)

                if resultados_formatados:
                    context['resultado'] = resultados_formatados
                else:
                    context['erro'] = "Nenhuma doença corresponde aos sintomas."

            except Exception as e:
                context['erro'] = f"Erro na inferência: {e}"

    return render(request, 'index.html', context)