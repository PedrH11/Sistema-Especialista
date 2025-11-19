from pyswip import Prolog


def main():
    prolog = Prolog()

    # Carrega a base de conhecimento
    try:
        prolog.consult("knowledge_base.pl")
    except Exception as e:
        print(f"ERRO: Não foi possível carregar 'knowledge_base.pl'. {e}")
        print("Certifique-se que o SWI-Prolog está instalado e no PATH.")
        return

    # Pergunta 1: Quem é o pai de Ana?
    query = "pai(X, ana)"
    solucoes = list(prolog.query(query))

    if solucoes:
        # A solução vem como um dicionário: [{'X': 'joao'}]
        pai_ana = solucoes[0]["X"]
        print(f"O pai de Ana é: {pai_ana}")
    else:
        print("Não foi possível encontrar o pai de Ana.")

    # Pergunta 2: Quem são os netos de João? (Usando a regra 'avo')
    query_netos = "avo(joao, Neto)"
    netos = list(prolog.query(query_netos))

    print("\nNetos de João:")
    if netos:
        for solucao in netos:
            print(f"- {solucao['Neto']}")
    else:
        print("João não tem netos (segundo esta base).")


if __name__ == "__main__":
    main()
