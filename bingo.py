# Trabalho Prático 2 - Linguagens de Programação

# Autores: Yagho Mattos da Rocha - Matrícula: 201765565AC
#          Thiago de Almeida Lopes  - Matrícula: 201765556AC

import random

# ----------------------- Definições ---------------------------

def imprime_cartela_aux_item(lista_item, num_sorteado, n):
    if(n == 0):
        return []
    head_item = lista_item[0]
    tail_item = lista_item[1:]
    if head_item['ja_saiu'] == True:
        saiu = "X"
    else:
        saiu = " "
    if head_item['numero'] < 10:
        print(str(head_item['numero']) + "  ["+saiu+"]", end ="\t")
    else:
        print(str(head_item['numero']) + " ["+saiu+"]", end ="\t")

    imprime_cartela_aux_item(tail_item, num_sorteado, n-1)


def imprime_cartela_aux_linha(lista_linha, num_sorteado, n):
    if(n == 0):
        return
    
    head_linha = lista_linha[0]
    tail_linha = lista_linha[1:]
    imprime_cartela_aux_item(
        head_linha, 
        num_sorteado, 
        len(head_linha)
    )
    print("")
    imprime_cartela_aux_linha(tail_linha, num_sorteado, n-1)


def imprime_cartela(lista_jogadores, num_sorteado, n):
    if(n==0):
        return
    if(len(lista_jogadores)==n and num_sorteado > 0):
        print("============================= Valor Sorteado: "+str(num_sorteado)+" ============================")
    print("Cartelas:")
    head_jogador = lista_jogadores[0]
    tail_jogador = lista_jogadores[1:]

    if head_jogador['tipo_cartela'] == 'l':
        print ("============================= "+head_jogador['nome']+" : linha ===================================")
    else:
        print ("============================= "+head_jogador['nome']+" : coluna ===================================")
    
    imprime_cartela_aux_linha(
        head_jogador['cartela'], 
        num_sorteado, 
        len(head_jogador['cartela'])
    )
    imprime_cartela(tail_jogador, num_sorteado, n-1)

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

    if tipo_cartela != 'l' and tipo_cartela != 'c':
        print("Tipo de cartela inválido. Digite ( l ) para linha e ( c ) para coluna.")
        inicia_jogadores_aux(num_jogadores, jogador)

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

##############################
# Faz a atualização da cartela 
##############################

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
    return [{'nome':head_jogador['nome'], 'tipo_cartela': head_jogador['tipo_cartela'], 'cartela': new_cartela }] + preenche_cartela(tail_jogador, num_sorteado, n-1)

##################################
# Verifica se algum jogador ganhou 
##################################

def verifica_vitoria_aux_linha2(lista_item, n):
    if(n == 0):
        return True
    head_item = lista_item[0]
    tail_item = lista_item[1:]
    return head_item['ja_saiu'] and verifica_vitoria_aux_linha2(tail_item, n-1)

def verifica_vitoria_aux_linha(lista_linha, n):
    if(n == 0):
        return False
    head_linha = lista_linha[0]
    tail_linha = lista_linha[1:]
    is_vencedor = verifica_vitoria_aux_linha2(head_linha, len(head_linha))
    return is_vencedor or verifica_vitoria_aux_linha(tail_linha, n-1)

def verifica_vitoria (lista_jogadores, n):
    if(n==0):
        return []
    head_jogador = lista_jogadores[0]
    tail_jogador = lista_jogadores[1:]
    tipo = head_jogador['tipo_cartela']
    if tipo == 'l':
        is_vencedor = verifica_vitoria_aux_linha(head_jogador['cartela'], len(head_jogador['cartela']))
    else:
        # tem uma função transpose no bingo.lhs que eu copiei do stack overflow
        cartela_transposta = list(map(list, zip(*head_jogador['cartela']))) 
        is_vencedor = verifica_vitoria_aux_linha(cartela_transposta, len(cartela_transposta))
    if is_vencedor == True:
        return [head_jogador['nome']] + verifica_vitoria(tail_jogador, n-1)
    else:
        return verifica_vitoria(tail_jogador, n-1)
    


def calcula_turno(game_state):
    old_jogadores = game_state['jogadores']
    new_rodada = game_state['rodada'] + 1
    old_numeros_sorteados = game_state['numeros_sorteados']
    old_numeros_para_sorteio = game_state['numeros_para_sorteio']
    old_vencedores = game_state['vencedores']
    qtd_para_sorteio = len(old_numeros_para_sorteio)

    if qtd_para_sorteio <= 0 or new_rodada > 75 or len(old_vencedores) > 0:
        if len(old_vencedores) > 0:
            game_over(game_state)
        return
    else:
        num_sorteado = old_numeros_para_sorteio[0]
        new_numeros_para_sorteio = old_numeros_para_sorteio[1:]
        new_numeros_sorteados = [num_sorteado] + old_numeros_sorteados
        print("******* Rodada:", new_rodada, "Numero sorteado:", num_sorteado,'*********')

        new_jogadores = preenche_cartela(old_jogadores, num_sorteado, len(old_jogadores))

        imprime_cartela(new_jogadores, num_sorteado, len(new_jogadores))

        new_vencedores = verifica_vitoria(new_jogadores, len(new_jogadores))

        calcula_turno({
            'jogadores': new_jogadores,
            'rodada': new_rodada,
            'numeros_sorteados': new_numeros_sorteados,
            'numeros_para_sorteio': new_numeros_para_sorteio,
            'vencedores': new_vencedores
        })

# ----------------------- Finalizacao --------------------------

def game_over(game_state):
    vencedores = game_state['vencedores']
    print("Parabéns:",vencedores)
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
            'numeros_para_sorteio': new_numeros_para_sorteio,
            'vencedores': []
        })
        return


main()
# imprime_cartela(
#     [
#         {
#             'nome': 'joão',
#             'tipo_cartela': 'l',
#             'cartela': [
#                 [{'numero': 1, 'ja_saiu': False}, {'numero': 4, 'ja_saiu': False}],
#                 [{'numero': 2, 'ja_saiu': False}, {'numero': 5, 'ja_saiu': True}],
#                 [{'numero': 3, 'ja_saiu': True}, {'numero': 6, 'ja_saiu': False}]
#             ]
#         },
#         {
#             'nome': 'maria',
#             'tipo_cartela': 'c',
#             'cartela': [
#                 [{'numero': 2, 'ja_saiu': False}, {'numero': 3, 'ja_saiu': True}],
#                 [{'numero': 5, 'ja_saiu': True}, {'numero': 7, 'ja_saiu': False}],
#                 [{'numero': 9, 'ja_saiu': False}, {'numero': 11, 'ja_saiu': False}]
#             ]
#         }
#     ],
#     9,
#     2
# )

