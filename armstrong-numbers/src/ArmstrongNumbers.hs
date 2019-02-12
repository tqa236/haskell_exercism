module ArmstrongNumbers (armstrong) where

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

armstrong :: Integral a => a -> Bool
armstrong a = sum ( map ( ^ length digits) digits) == a
    where digits = digs a
