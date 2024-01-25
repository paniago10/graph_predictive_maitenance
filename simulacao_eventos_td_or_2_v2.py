import random
import math
import networkx as nx
import pandas as pd
import numpy as np

# Definindo o grafo
G = nx.Graph()
G.add_nodes_from(range(1, 13))
edges = [
    (1, 8), (2, 8), (3, 8),
    (1, 9), (4, 9),
    (1, 10), (5, 10),
    (1, 11), (6, 11), (7, 11),
    (8, 12), (9, 12), (10, 12), (11, 12)
]
G.add_edges_from(edges)

# Inicializando as variáveis
tempo_simulacao = 0
resultados = []

# Função para inicializar o grafo com os estados iniciais por probabilidade
def initialize():
    global G, tempo_simulacao
    node_probabilities = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]

    for node, probability in enumerate(node_probabilities, start=1):
        if random.random() < probability:
            G.nodes[node]['state'] = 'em funcionamento'
        else:
            G.nodes[node]['state'] = 'falha'

    tempo_simulacao = 0

# Função para atualizar o grafo com base nas probabilidades dinâmicas
def update():
    global G, tempo_simulacao, resultados

    node_probabilities = [
        1 - (math.exp(-np.random.exponential(scale=1 / 2009.143, size=1) * tempo_simulacao)),
        1 - (math.exp(-np.random.exponential(scale=1 / 3230.000000, size=1) * tempo_simulacao)),
        1 - (math.exp(-np.random.exponential(scale=1 / 4566.857143, size=1) * tempo_simulacao)),
        1 - (math.exp(-np.random.exponential(scale=1 / 5358.857143, size=1) * tempo_simulacao)),
        1 - (math.exp(-np.random.exponential(scale=1 / 12672.00000, size=1) * tempo_simulacao)),
        1 - (math.exp(-np.random.exponential(scale=1 / 6667.200000, size=1) * tempo_simulacao)),
        1 - (math.exp(-np.random.exponential(scale=1 / 1564.800000, size=1) * tempo_simulacao))
    ]

    for node, probability in enumerate(node_probabilities, start=1):
        random_value = np.random.uniform(0.97, 0.99)
        #print(f"Node {node} - Probability: {probability}, Random Value: {random_value}")

        # Testa se o valor aleatório está abaixo da probabilidade calculada
        if G.nodes[node]['state'] == 'em funcionamento' and random_value < probability:
            G.nodes[node]['state'] = 'falha'
        elif G.nodes[node]['state'] == 'falha' and random_value < probability:
            G.nodes[node]['state'] = 'em funcionamento'

    # Atualiza o tempo da simulação
    tempo_simulacao += 1

    # Implemente a lógica do "gate or" para o nó 8
    if any(G.nodes[node]['state'] == 'falha' for node in [1, 2, 3]):
        G.nodes[8]['state'] = 'falha'
    else:
        G.nodes[8]['state'] = 'em funcionamento'

    # Implemente a lógica do "gate or" para o nó 9
    if any(G.nodes[node]['state'] == 'falha' for node in [1, 4]):
        G.nodes[9]['state'] = 'falha'
    else:
        G.nodes[9]['state'] = 'em funcionamento'

    # Implemente a lógica do "gate or" para o nó 10
    if any(G.nodes[node]['state'] == 'falha' for node in [1, 5]):
        G.nodes[10]['state'] = 'falha'
    else:
        G.nodes[10]['state'] = 'em funcionamento'

    # Implemente a lógica do "gate or" para o nó 11
    if any(G.nodes[node]['state'] == 'falha' for node in [1, 6, 7]):
        G.nodes[11]['state'] = 'falha'
    else:
        G.nodes[11]['state'] = 'em funcionamento'

    # Implemente a lógica do "gate or" para o nó 12
    if any(G.nodes[node]['state'] == 'falha' for node in [8, 9, 10, 11]):
        G.nodes[12]['state'] = 'falha'
    else:
        G.nodes[12]['state'] = 'em funcionamento'

    # Verifique se o nó 12 entrou em falha
    if G.nodes[12]['state'] == 'falha':
        #print(f"O ventilador apresentou uma falha e deixou de funcionar após {tempo_simulacao} horas.")
        
        # Adiciona o tempo de falha aos resultados
        resultados.append(tempo_simulacao)

# Mapeamento de cores para estados
color_mapping = {'em funcionamento': 'blue', 'falha': 'red'}

# Função para salvar os resultados no arquivo Excel
def salvar_resultados():
    # Cria um DataFrame com os resultados
    df = pd.DataFrame(resultados, columns=['Tempo de Falha'])

    # Salva o DataFrame em um arquivo Excel
    df.to_excel('D:\\PythonScripts\\complex\\IC\\resultados_simulacao.xlsx', index=False)

# Função para realizar a simulação completa
def simulacao_completa(replicacoes=500):
    for _ in range(replicacoes):
        initialize()
        while G.nodes[12]['state'] != 'falha':
            update()

        # Atualiza a visualização do grafo
        #print(f"Estado final do grafo após a replicação {_ + 1}:")
        #for node in G.nodes:
        #    print(f"Node {node} - State: {G.nodes[node]['state']}")
        #print()

        # Salva os resultados após cada replicação
        salvar_resultados()

# Chama a função para realizar a simulação completa
simulacao_completa()


