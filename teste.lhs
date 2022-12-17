> import System.Random

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
