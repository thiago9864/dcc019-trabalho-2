# Trabalho Prático 2 - Linguagens de Programação

# Autores: Yagho Mattos da Rocha - Matrícula: 201765565AC
#          Thiago de Almeida Lopes  - Matrícula: 201765556AC

import random

# ----------------------- Definições ---------------------------


# ---------------------- Inicialização -------------------------


def inicia_jogadores(num_jogadores):
    return inicia_jogadores_aux(num_jogadores, 1)


def inicia_jogadores_aux(num_jogadores, jogador):
    if num_jogadores < jogador:
        return []

    print("Qual o nome do jogador " + str(jogador) + "?")
    nome = input()
    print("Digite o tipo da cartela de " + nome +
          " -- ( l ) para linha e ( c ) para coluna: ")
    tipo_cartela = input()

    l1 = list(range(1, 16))
    l2 = list(range(16, 31))
    l3 = list(range(31, 46))
    l4 = list(range(46, 61))
    l5 = list(range(61, 75))

    random.shuffle(l1)
    random.shuffle(l2)
    random.shuffle(l3)
    random.shuffle(l4)
    random.shuffle(l5)

    cartela = [
        linha_cartela(5, l1),
        linha_cartela(5, l2),
        linha_cartela(5, l3),
        linha_cartela(5, l4),
        linha_cartela(5, l5)
    ]

    return [{'nome': nome, 'tipo_cartela': tipo_cartela, 'cartela': cartela}] + inicia_jogadores_aux(num_jogadores, jogador+1)


def linha_cartela(n, lista):
    if n < 1:
        return []
    else:
        x = lista[0]  # Cabeça da lista
        xs = lista[1:]  # cauda da lista
        return [{'numero': x, 'ja_saiu': False}] + linha_cartela(n-1, xs)

# ------------------------- Turnos -----------------------------


def preenche_cartela_aux_item(lista_item, num_sorteado, n):
    if(n == 0):
        return []
    head_item = lista_item[0]
    tail_item = lista_item[1:]
    #print(head_item, tail_item, n)
    if(head_item['numero'] == num_sorteado):
        return [{'numero': head_item['numero'], 'ja_saiu': True}] + preenche_cartela_aux_item(tail_item, num_sorteado, n-1)
    else:
        return [{'numero': head_item['numero'], 'ja_saiu': head_item['ja_saiu']}] + preenche_cartela_aux_item(tail_item, num_sorteado, n-1)


def preenche_cartela_aux_linha(lista_linha, num_sorteado, n):
    if(n == 0):
        return []
    head_linha = lista_linha[0]
    tail_linha = lista_linha[1:]
    nova_linha = preenche_cartela_aux_item(
        head_linha, num_sorteado, len(head_linha))

    return [nova_linha] + preenche_cartela_aux_linha(tail_linha, num_sorteado, n-1)


def preenche_cartela(lista_jogadores, num_sorteado, n):
    if(n==0):
        return []
    head_jogador = lista_jogadores[0]
    tail_jogador = lista_jogadores[1:]
    new_cartela = preenche_cartela_aux_linha(
        head_jogador['cartela'], num_sorteado, len(head_jogador['cartela']))
    return [{'cartela': new_cartela }] + preenche_cartela(tail_jogador, num_sorteado, n-1)


def calcula_turno(game_state):
    old_jogadores = game_state['jogadores']
    new_rodada = game_state['rodada'] + 1
    old_numeros_sorteados = game_state['numeros_sorteados']
    old_numeros_para_sorteio = game_state['numeros_para_sorteio']
    qtd_para_sorteio = len(old_numeros_para_sorteio)

    if qtd_para_sorteio <= 0 or new_rodada > 75:
        return
    else:
        num_sorteado = old_numeros_para_sorteio[0]
        new_numeros_para_sorteio = old_numeros_para_sorteio[1:]
        new_numeros_sorteados = [num_sorteado] + old_numeros_sorteados
        print("Rodada:", new_rodada, "Numero sorteado:", num_sorteado)

        new_jogadores = preenche_cartela(old_jogadores, num_sorteado, len(old_jogadores))

        calcula_turno({
            'jogadores': new_jogadores,
            'rodada': new_rodada,
            'numeros_sorteados': new_numeros_sorteados,
            'numeros_para_sorteio': new_numeros_para_sorteio
        })

# ----------------------- Finalizacao --------------------------

# ------------------------ Main Loop ---------------------------


def main():
    print("Informe a quantidade de jogadores: ")
    line = input()
    num_jogadores = int(line)
    if num_jogadores < 2 or num_jogadores > 4:
        print("Número de jogadores invalido!")
        main()
    else:
        new_jogadores = inicia_jogadores(num_jogadores)
        new_rodada = 0
        new_numeros_sorteados = []
        new_numeros_para_sorteio = list(range(1, 76))
        random.shuffle(new_numeros_para_sorteio)
        calcula_turno({
            'jogadores': new_jogadores,
            'rodada': new_rodada,
            'numeros_sorteados': new_numeros_sorteados,
            'numeros_para_sorteio': new_numeros_para_sorteio
        })
        return


# main()
print(
    preenche_cartela(
        [
            {
                'cartela': [
                    [{'numero': 1, 'ja_saiu': False}, {'numero': 4, 'ja_saiu': False}],
                    [{'numero': 2, 'ja_saiu': False}, {'numero': 5, 'ja_saiu': True}],
                    [{'numero': 3, 'ja_saiu': True}, {'numero': 6, 'ja_saiu': False}]
                ]
            },
            {
                'cartela': [
                    [{'numero': 2, 'ja_saiu': False}, {'numero': 3, 'ja_saiu': True}],
                    [{'numero': 5, 'ja_saiu': True}, {'numero': 7, 'ja_saiu': False}],
                    [{'numero': 9, 'ja_saiu': False}, {'numero': 11, 'ja_saiu': False}]
                ]
            }
        ],
        9,
        2
    )
)
