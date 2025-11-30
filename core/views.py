from django.shortcuts import render
from pyswip import Prolog
from django.conf import settings
import os
import google.generativeai as genai
import markdown 

# ================= CONFIGURAÇÕES =================
# 1. Configuração do Prolog
KB_PATH = os.path.join(settings.BASE_DIR, 'knowledge_base.pl')
prolog_engine = Prolog()
try:
    prolog_engine.consult(KB_PATH)
except Exception as e:
    prolog_engine = None

# 2. Configuração do Gemini
GOOGLE_API_KEY = "" # Insira sua chave API do Google aqui
genai.configure(api_key=GOOGLE_API_KEY)


# ================= FUNÇÕES AUXILIARES =================
def corrigir_texto(texto_cru):
    """Corrige acentuação (Mojibake) do Prolog"""
    if isinstance(texto_cru, bytes): return texto_cru.decode('utf-8', errors='ignore')
    if isinstance(texto_cru, str):
        try: return texto_cru.encode('latin-1').decode('utf-8')
        except: return texto_cru
    return str(texto_cru)

def consultar_gemini(sintomas_lista):
    """Envia os sintomas para o Gemini e pede um diagnóstico"""
    chave_limpa = str(GOOGLE_API_KEY).strip()

    if not chave_limpa or "COLE_SUA" in chave_limpa:
        return "⚠️ Erro: A chave API ainda não foi configurada corretamente."

    try:
        genai.configure(api_key=chave_limpa)
        model = genai.GenerativeModel('gemini-2.5-flash-lite')
        
        prompt = f"""
        Aja como um sistema de diagnóstico médico de precisão.
        Entrada: {', '.join(sintomas_lista)}.
        
        Regras de Resposta:
        1. NÃO use introduções como "Prezado paciente" ou "Com base nos sintomas".
        2. Vá direto para a lista de diagnósticos.
        3. Use formatação Markdown clara.
        
        Saída esperada:
        ### 1. [Nome da Doença Mais Provável]
        * **Justificativa:** [Explicação curta e técnica]
        * **Recomendação:** [Ação imediata]
        
        ### 2. [Nome da Segunda Hipótese]
        ...
        """
        
        response = model.generate_content(prompt)
        return markdown.markdown(response.text)
        
    except Exception as e:
        return f"Erro Gemini: {str(e)}"

# ================= VIEW PRINCIPAL =================
def consulta_view(request):
    context = {
        'resultado_prolog': None,
        'resultado_gemini': None, 
        'erro': None,
        'sintomas_db': [],
        'sintomas_selecionados': []
    }

    if not prolog_engine:
        context['erro'] = "Motor Prolog não inicializado."
        return render(request, 'index.html', context)

    # --- CARREGAR SINTOMAS (GET) ---
    try:
        consulta_sintomas = list(prolog_engine.query("todos_sintomas(Lista)"))
        if consulta_sintomas:
            lista_suja = consulta_sintomas[0]['Lista']
            sintomas_limpos = []
            for s in lista_suja:
                s_str = str(s)
                texto_legivel = corrigir_texto(s_str).replace('_', ' ').title()
                sintomas_limpos.append({'valor': s_str, 'texto': texto_legivel})
            
            sintomas_limpos.sort(key=lambda x: x['texto'])
            context['sintomas_db'] = sintomas_limpos
    except Exception as e:
        context['erro'] = f"Erro ao carregar sintomas: {e}"

    # --- PROCESSAR DIAGNÓSTICO (POST) ---
    if request.method == 'POST':
        # Captura Sintomas E Fatores de Risco
        selecionados = request.POST.getlist('sintomas')
        fatores_selecionados = request.POST.getlist('fatores') # 
        
        context['sintomas_selecionados'] = selecionados

        if not selecionados:
             context['erro'] = "Selecione ao menos um sintoma."
        else:
            # 1. CONSULTA PROLOG (IA Simbólica)
            try:
                lista_sintomas_str = "[" + ",".join(selecionados) + "]"
                lista_fatores_str = "[" + ",".join(fatores_selecionados) + "]" 
                
                query = f"diagnostico_completo(Doenca, {lista_sintomas_str}, {lista_fatores_str}, Pontos)"
                
                solucoes = list(prolog_engine.query(query))
                
                resultados_fmt = []
                for sol in solucoes:
                    nome_cru = str(sol['Doenca'])
                    
                    item = {
                        'nome': nome_cru.capitalize(),
                        'nome_raw': nome_cru,
                        'pontos': sol['Pontos'],
                        'conselho': None,
                        'gravidade': 'rotina',
                        'faltantes': [] 
                    }

                    # 1. Buscar Gravidade
                    try:
                        q_grav = list(prolog_engine.query(f"gravidade({nome_cru}, G)"))
                        if q_grav: item['gravidade'] = str(q_grav[0]['G'])
                    except: pass

                    # 2. Buscar Conselho 
                    try:
                        q_faltantes = list(prolog_engine.query(f"sintomas_faltantes({nome_cru}, {lista_sintomas_str}, L)"))
                        if q_faltantes:
                            lista_crua = q_faltantes[0]['L']
                            lista_limpa = []
                            for s in lista_crua:
                                s_str = corrigir_texto(str(s))
                                s_bonito = s_str.replace('_', ' ').title()
                                lista_limpa.append(s_bonito)
                            item['faltantes'] = lista_limpa[:4]
                    except Exception as e:
                        print(f"Erro detetive: {e}")

                    resultados_fmt.append(item)

                resultados_fmt.sort(key=lambda x: x['pontos'], reverse=True)

                if resultados_fmt:
                    vencedor = resultados_fmt[0]
                    try:
                        q_conselho = list(prolog_engine.query(f"conselho({vencedor['nome_raw']}, X)"))
                        if q_conselho:
                            vencedor['conselho'] = corrigir_texto(q_conselho[0]['X'])
                    except: pass
                    
                    context['resultado_prolog'] = resultados_fmt
                else:
                    context['resultado_prolog'] = []

            except Exception as e:
                context['erro'] = f"Erro no Prolog: {e}"

            # 2. CONSULTA GEMINI (IA Generativa)
            sintomas_legiveis = [s.replace('_', ' ') for s in selecionados]
            fatores_legiveis = [f.replace('_', ' ') for f in fatores_selecionados]
            
            # Cria uma lista única para o prompt da IA
            lista_completa_ia = sintomas_legiveis + [f"(Fator: {f})" for f in fatores_legiveis]
            
            context['resultado_gemini'] = consultar_gemini(lista_completa_ia)

    return render(request, 'index.html', context)