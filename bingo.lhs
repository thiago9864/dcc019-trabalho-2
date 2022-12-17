Trabalho Prático 2 - Linguagens de Programação

Autores: Yagho Mattos da Rocha - Matrícula: 201765565AC 
         Thiago de Almeida Lopes  - Matrícula: 201765556AC

----------------------- Definições ---------------------------

> import System.Random

Tipo que contem um inteiro que recebe o numero da posição da cartela
e um boolean que indica se o número já foi sorteado ou não.

> data ItemCartela = ItemCartela {
>     numero :: Int,
>     ja_saiu :: Bool
> } deriving (Eq, Show, Read)

Tipo que contem um string que recebe o nome do jogador, outro string que recebe
o caracter que indica se a avaliação de vitoria do jogador sob a cartela, e a
propria cartela, que é uma matriz de elementos do tipo ItemCartela

> data Jogador = Jogador {
>     nome :: String,
>     tipo_cartela :: String,
>     cartela :: [[ItemCartela]]
> } deriving (Eq, Show, Read)

Tipo que contem uma lista de jogadores, o inteiro que guarda qual rodada o jogo está
uma lista que armazena todos os inteiros sorteados, que são retirados da ultima lista
que inicia toda preenchida com numeros de 1 a 75

> data Game = Game {
>    jogadores :: [Jogador],
>    rodada :: Int,
>    numeros_sorteados :: [Int],
>    numeros_para_sorteio :: [Int]
> } deriving (Eq, Show, Read)

Algoritmo para embaralhar listas
Fonte: https://stackoverflow.com/questions/14692059/how-to-shuffle-a-list

> shuffle gen [] = []
> shuffle gen list = randomElem : shuffle newGen newList
>   where
>      randomTuple = randomR (0, (length list) - 1) gen
>      randomIndex = fst randomTuple
>      newGen = snd randomTuple
>      randomElem = list !! randomIndex
>      newList = take randomIndex list ++ drop (randomIndex+1) list

---------------------- Inicialização -------------------------

Recebe uma lista de numeros inteiros e retorna os 5 primeiros valores também como uma lista

> linha_cartela :: Int -> [Int] -> [ItemCartela]
> linha_cartela n (x:xs)
>        | n < 1 = []
>        | otherwise = [(ItemCartela x False)] ++ (linha_cartela (n-1) xs)


Função auxiliar que colhe o nome e tipo da cartela e também cria a cartela do jogador

> inicia_jogadores_aux :: Int -> Int -> IO([Jogador])
> inicia_jogadores_aux num_jogadores num_jogador = do
>    if num_jogadores < num_jogador then
>        do return ([])
>    else  
>        do  
>        putStrLn ("Qual o nome do jogador " ++ (show num_jogador) ++ "?")
>        nome_jogador <- getLine
>        -- let nome_jogador = read linha :: [Char]
>        putStrLn ("Digite o tipo da cartela de " ++ nome_jogador ++ " -- ( l ) para linha e ( c ) para coluna: ")
>        tp_cartela <- getLine
>        -- let tp_cartela = read linha :: [Char]
>        rndIO <- randomIO
>        let cartela = [ (linha_cartela 5 (shuffle (mkStdGen rndIO) [1..15]))
>                      , (linha_cartela 5 (shuffle (mkStdGen rndIO) [16..30]))
>                      , (linha_cartela 5 (shuffle (mkStdGen rndIO) [31..45]))
>                      , (linha_cartela 5 (shuffle (mkStdGen rndIO) [46..60]))
>                      , (linha_cartela 5 (shuffle (mkStdGen rndIO) [61..75])) 
>                      ] 
>        let novo_jogador = [(Jogador nome_jogador tp_cartela cartela)]
>        jogadores_anteriores <- (inicia_jogadores_aux num_jogadores (num_jogador + 1))
>        return (novo_jogador ++ jogadores_anteriores)

Função principal, chamada na main que cria os jogadores e retorna uma lista de jogadores

> inicia_jogadores :: Int -> IO([Jogador])
> inicia_jogadores num_jogadores = (inicia_jogadores_aux num_jogadores 1)
    

------------------------- Turnos -----------------------------

----------------------- Finalizacao --------------------------

------------------------ Main Loop ---------------------------

> main :: IO ()
> main = do 
>        putStrLn "Informe a quantidade de jogadores: "
>        linha <- getLine
>        let num_jogadores = read linha :: Int
>        if num_jogadores < 2 || num_jogadores > 4 then
>           do
>            putStrLn "\nErro: Número de jogadores invalido!"
>            main
>        else
>           return ()
