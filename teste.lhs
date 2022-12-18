> import System.Random

> data ItemCartela = ItemCartela {
>     numero :: Int,
>     ja_saiu :: Bool
> } deriving (Eq, Show, Read)

> data Jogador = Jogador {
>     nome :: String,
>     tipo_cartela :: String,
>     cartela :: [[ItemCartela]]
> } deriving (Eq, Show, Read)

> shuffle gen [] = []
> shuffle gen list = randomElem : shuffle newGen newList
>   where
>      randomTuple = randomR (0, (length list) - 1) gen
>      randomIndex = fst randomTuple
>      newGen = snd randomTuple
>      randomElem = list !! randomIndex
>      newList = take randomIndex list ++ drop (randomIndex+1) list

> main = do
>      r <- randomIO
>      let lst = [1,2,3,4,5]
>      putStrLn (show (shuffle (mkStdGen r) lst))

> lista = [ (shuffle (mkStdGen 5) [1,2,3,4,5])
>         , (shuffle (mkStdGen 5) [1,2,3,4,5])
>         ]

> data Game = Game {
>    rodada :: Int,
>    lista2 :: [Int]
> } deriving (Eq, Show, Read)

> calcula_turno :: Game -> IO ()
> calcula_turno game_state = do 
>                let nova_rodada = rodada(game_state) + 1
>                let nova_lista = lista2(game_state)
>                putStrLn ("Rodada " ++ (show nova_rodada) ++ " Len lista " ++ (show (length nova_lista)))
>                if nova_rodada > 5 then 
>                    return ()
>                else 
>                    calcula_turno (Game nova_rodada nova_lista)

> preenche_cartela_aux_item :: [ItemCartela] -> Int -> Int -> IO([ItemCartela])
> preenche_cartela_aux_item lista_item num_sorteado n = do 
>                if n == 0 then 
>                    return []
>                else
>                    do
>                    let head_item = (head lista_item)
>                    let tail_item = (tail lista_item)
>                    lista_rec <- (preenche_cartela_aux_item tail_item num_sorteado (n-1))
>                    if numero(head_item) == num_sorteado then
>                        return ([(ItemCartela num_sorteado True)] ++ lista_rec)
>                    else 
>                        return ([head_item] ++ lista_rec)

> preenche_cartela_aux_linha :: [[ItemCartela]] -> Int -> Int -> IO([[ItemCartela]])
> preenche_cartela_aux_linha lista_linha num_sorteado n = do 
>                if n == 0 then 
>                    return []
>                else
>                    do
>                    let head_linha = (head lista_linha)
>                    let tail_linha = (tail lista_linha)
>                    nova_linha <- (preenche_cartela_aux_item head_linha num_sorteado 5)
>                    lista_rec <- (preenche_cartela_aux_linha tail_linha num_sorteado (n-1))
>                    return ([nova_linha] ++ lista_rec)

> preenche_cartela :: [Jogador] -> Int -> Int -> IO([Jogador])
> preenche_cartela lista_jogadores num_sorteado n = do 
>                if n == 0 then 
>                    return []
>                else
>                    do
>                    let head_jogador = (head lista_jogadores)
>                    let tail_jogador = (tail lista_jogadores)
>                    let old_cartela = cartela(head_jogador)
>                    let nome_jogador = nome(head_jogador)
>                    let tipo_cartela_jogador = tipo_cartela(head_jogador)
>                    nova_cartela <- (preenche_cartela_aux_linha old_cartela num_sorteado 5)
>                    lista_rec <- (preenche_cartela tail_jogador num_sorteado (n-1))
>                    return ([(Jogador nome_jogador tipo_cartela_jogador nova_cartela)] ++ lista_rec)
