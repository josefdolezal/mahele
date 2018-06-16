{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import qualified Language.Mahele.Parser as P
import qualified Language.Swift.Pretty as P
import           Turtle
import qualified Data.Text as T

inputFileArgument :: Parser T.Text
inputFileArgument = argText "src" "Mahele source file"

main :: IO ()
main = do
    input <- options "Model files generator based on Mahele declaration" inputFileArgument
    result <- P.parseMahele <$> readFile (T.unpack input)
    case result of
        Left e  -> print e
        Right m -> putStrLn . unlines $ map P.prettySwift m
