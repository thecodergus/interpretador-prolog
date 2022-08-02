module Main where
import Data.Text.Titlecase
import Control.Monad.State
import System.IO
import System.Environment
import Parser
import Interpretador

main :: IO ()
main = do
		print $ "Digite o nome do arquivo prolog(com extesao e deve estar no mesmo diretorio de 'Main.hs'):"
		file_name <- getLine
		-- let file_name = "cores.pl"
		codigo_prolog <- readFile file_name
		let prog = parse programa codigo_prolog

		putStrLn $ "Nome arquivo: " ++ file_name
		putStrLn $ "Carregado: " ++ (show $ length prog) ++ " regras"
		putStrLn $ concat (map (\c -> " " ++ (show c) ++ "\n") prog)
		putStrLn $ "Pronto"

		loop prog


loop :: Programa -> IO ()
loop prog = do
			putStrLn $ "Digite: "
			hFlush stdout
			input <- getLine
			let goal = parse termo input

			let (substs, _) = runState (reach goal) (EstadoExecucao prog 0)
			if substs /= []  
				then putStrLn $ concat $ map (\s -> ((showSubstVars s (varNames goal)) ++ "\n")) $ substs
				else putStrLn "false." >> putStrLn ""
				
			

			loop prog
			