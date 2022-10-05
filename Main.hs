module Main where

import Lexer
import Parser
import Interm
import Assembly
import qualified Data.Map as Map
import           Control.Monad.State

main :: IO()
main = do
		txt <- getContents
		let toks = alexScanTokens txt
		print toks
		let ast = parser toks
		print ast
		let inter = evalState (transProg Map.empty ast) (0,0)
		mapM_ print inter
		mapM_ print $ getAssembly inter