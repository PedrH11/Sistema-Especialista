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
    # Dicionário de "contexto" para enviar dados para o HTML
    context = {
        'resultado': None,
        'erro': None,
        's1': '', # Para manter os valores nos inputs
        's2': '',
    }

    # Verifica se o motor Prolog foi carregado
    if not prolog_engine:
        context['erro'] = "Motor Prolog não inicializado. Verifique a consola."
        return render(request, 'index.html', context)

    # --- PASSO B: Processar o Formulário (se foi enviado) ---
    if request.method == 'POST':
        # 1. Obter os dados do formulário (dos inputs 'name="sintoma1"')
        s1 = request.POST.get('sintoma1', '').strip().lower()
        s2 = request.POST.get('sintoma2', '').strip().lower()
        
        # Guarda os valores para mostrar de volta no form
        context['s1'] = s1
        context['s2'] = s2

        if not s1 or not s2:
            context['erro'] = "Por favor, preencha os dois sintomas."
        else:
            try:
                # 2. Formatar a consulta Prolog (Ex: "diagnostico(Doenca, febre, tosse)")
                query = f"diagnostico(Doenca, {s1}, {s2})"
                
                # 3. Executar a consulta
                #    list() força o pyswip a encontrar todas as soluções
                solucoes = list(prolog_engine.query(query))
                
                context['solucoes_raw'] = solucoes # Para debug

                if solucoes:
                    # 4. Formatar a resposta (Python)
                    #    Pyswip retorna: [{'Doenca': 'gripe'}]
                    diagnostico = solucoes[0]['Doenca']
                    context['resultado'] = diagnostico
                else:
                    context['erro'] = "Nenhuma doença encontrada com essa combinação."

            except Exception as e:
                # Captura erros de sintaxe (ex: "febre@")
                context['erro'] = f"Sintoma inválido ou erro na consulta: {e}"

    # --- PASSO C: Renderizar a Página ---
    # Se for um GET (primeira visita) ou se for um POST (depois de processar),
    # ele renderiza o HTML, injetando o dicionário 'context'.
    return render(request, 'index.html', context)