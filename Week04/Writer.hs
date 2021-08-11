module Week04.Writer where

import Control.Monad
import Week04.Monad

data Writer a = Writer a [String]
    deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

tell :: [String] -> Writer ()
tell = Writer ()

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m
    log_s = ["sum: " ++ show s]
  in
    Writer s $ xs ++ ys ++ zs ++ log_s

bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []

foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
 -- 
-- foo'' x y z = do
--    s <- threeInts x y z
--    tell ["sum: " ++ show s]
--    return s
-- }
foo'' x y z = threeInts x y z >>= (\s -> Writer s ["sum: " ++ show s] )

instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
