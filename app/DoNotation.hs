module DoNotation (main) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Maybe

maybeMonad :: Maybe String
maybeMonad = do
    x <- Just "Hello"
    y <- Just "World"
    pure (x ++ " " ++ y)

eitherMonad :: Either String String
eitherMonad = do
    x <- Right "Hello"
    y <- Right "World"
    pure (x ++ " " ++ y)

readerMonad :: Reader (String, String) String
readerMonad = do
    x <- asks fst
    y <- asks snd
    pure (x ++ " " ++ y)

writerMonad :: Writer String String
writerMonad = do
    tell "Hello"
    tell " "
    tell "World"
    pure ""

stateMonad :: State String String
stateMonad = do
    put "Hello"
    x <- get
    put "World"
    y <- get
    pure (x ++ " " ++ y)

ioMonad :: IO String
ioMonad = do
    x <- getLine
    y <- getLine
    pure (x ++ " " ++ y)

main :: IO ()
main = do
    traverse_
        putStrLn
        [ runReader readerMonad ("Hello", "World")
        , either id id eitherMonad
        , fromMaybe "" maybeMonad
        , evalState stateMonad ""
        , execWriter writerMonad
        ]
    putStrLn "Please enter 'Hello' and 'World':"
    putStrLn =<< ioMonad
