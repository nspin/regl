nubOnInRangeThen :: forall i a b. Ix i => (i, i) -> (a -> i) -> [a] -> ([a] -> (i -> Bool) -> b) -> b
nubOnInRangeThen range key xs0 f = runST $ do
    set <- A.newArray range False :: ST s (A.STUArray s i Bool)
    let go acc [] = return acc
        go acc (x:xs) = do
            let k = key x
            seen <- A.readArray set k
            if seen
                then do
                    go acc xs
                else do
                    A.writeArray set k True
                    go (x:acc) xs
    xs <- go [] xs0
    frozenSet <- A.unsafeFreeze set
    return $ f xs (frozenSet A.!)
