from django.shortcuts import render
from pyswip import Prolog
from django.conf import settings
import os

# --- Inicialização do Motor (Igual ao anterior) ---
KB_PATH = os.path.join(settings.BASE_DIR, 'knowledge_base.pl')
prolog_engine = Prolog()

try:
    # O encoding='utf-8' aqui no open ajuda em alguns sistemas, 
    # mas o pyswip faz a leitura interna dele.
    prolog_engine.consult(KB_PATH)
    print(f"Base '{KB_PATH}' carregada.")
except Exception as e:
    print(f"ERRO: {e}")
    prolog_engine = None 


# --- FUNÇÃO AUXILIAR PARA CONSERTAR CARACTERES ---
def corrigir_texto(texto_cru):
    """
    Tenta consertar problemas de acentuação (Mojibake).
    Ex: Transforma 'hidrataÃ§Ã£o' em 'hidratação'.
    """
    if texto_cru is None:
        return ""
        
    # Se já vier como bytes (b'texto'), apenas decodifica
    if isinstance(texto_cru, bytes):
        return texto_cru.decode('utf-8', errors='ignore')
    
    # Se vier como string (str), tenta reverter a interpretação errada
    if isinstance(texto_cru, str):
        try:
            # O erro comum é ler UTF-8 como Latin-1. Vamos inverter isso.
            return texto_cru.encode('latin-1').decode('utf-8')
        except:
            # Se der erro (significa que o texto já estava certo ou é outro formato),
            # devolve o original.
            return texto_cru
            
    return str(texto_cru)


# --- VIEW PRINCIPAL ---
def consulta_view(request):
    context = {
        'resultado': None,
        'erro': None,
        'sintomas_db': [],
        'sintomas_selecionados': []
    }

    if not prolog_engine:
        context['erro'] = "Motor Prolog não inicializado."
        return render(request, 'index.html', context)

    # --- PARTE 1: Carregar Checkboxes ---
    try:
        consulta_sintomas = list(prolog_engine.query("todos_sintomas(Lista)"))
        if consulta_sintomas:
            lista_suja = consulta_sintomas[0]['Lista']
            sintomas_limpos = []
            for s in lista_suja:
                s_str = str(s) # O nome do sintoma geralmente não tem acento, mas se tiver:
                s_str = corrigir_texto(s_str) 
                
                texto_legivel = s_str.replace('_', ' ').title() 
                sintomas_limpos.append({'valor': s_str, 'texto': texto_legivel})
            
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
                lista_prolog_str = "[" + ",".join(selecionados) + "]"
                query = f"diagnostico_pontuado(Doenca, {lista_prolog_str}, Pontos)"
                
                solucoes = list(prolog_engine.query(query))
                
                resultados_formatados = []
                for sol in solucoes:
                    # Corrige o nome da doença (ex: 'intoxicacao' -> se tiver acento no futuro)
                    nome_cru = str(sol['Doenca'])
                    
                    resultados_formatados.append({
                        'nome': nome_cru.capitalize(),
                        'nome_raw': nome_cru, 
                        'pontos': sol['Pontos'],
                        'conselho': None
                    })
                
                # Ordena do maior para o menor
                resultados_formatados.sort(key=lambda x: x['pontos'], reverse=True)

                # Busca conselho APENAS para o Vencedor (Índice 0)
                if resultados_formatados:
                    vencedor = resultados_formatados[0]
                    
                    try:
                        q_conselho = list(prolog_engine.query(f"conselho({vencedor['nome_raw']}, X)"))
                        if q_conselho:
                            texto_cru = q_conselho[0]['X']
                            # AQUI ESTÁ A MÁGICA: Usa nossa função de correção
                            vencedor['conselho'] = corrigir_texto(texto_cru)
                            
                    except Exception as e:
                        print(f"Erro conselho: {e}")

                    context['resultado'] = resultados_formatados

                else:
                    context['erro'] = "Nenhuma doença encontrada."

            except Exception as e:
                context['erro'] = f"Erro na inferência: {e}"

    return render(request, 'index.html', context)