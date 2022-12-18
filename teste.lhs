> import System.Random
> import Control.Monad.State
> import Data.Functor.Identity


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
