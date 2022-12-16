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
    print("Digite o tipo da cartela de " + nome + " -- ( l ) para linha e ( c ) para coluna: ")
    tipo_cartela = input()

    l1 = list(range(1,16))
    l2 = list(range(16,31))
    l3 = list(range(31,46))
    l4 = list(range(46,61))
    l5 = list(range(61,75))

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
    
    return [{'nome' : nome, 'tipo_cartela': tipo_cartela, 'cartela': cartela}] + inicia_jogadores_aux(num_jogadores, jogador+1)

def linha_cartela(n, lista):
    if n < 1:
        return []
    else:
        x = lista[0] # Cabeça da lista
        xs = lista[1:] # cauda da lista
        return [{ 'numero': x, 'ja_saiu': False }] + linha_cartela(n-1, xs)

# ------------------------- Turnos -----------------------------

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
        print(inicia_jogadores(num_jogadores))
        return 

main()
