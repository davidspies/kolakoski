module Main where

import           Control.Monad                  ( forM_ )
import           System.Environment             ( getArgs )

import           Lib

main :: IO ()
main = do
  golist <- getArgs <&> \case
    []         -> twos
    [     arg] -> take (read arg) twos
    _ : _ :  _ -> error "Too many arguments"
  forM_ golist $ \x -> putStrLn $ unwords [show (nStarts x), show (startsLen x)]

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
