module Interpretador(
	reach,
	Subst,
	showSubstVars,
	varNames,
	EstadoExecucao (EstadoExecucao)
) where

import Control.Monad.State
import Data.List
import Data.Maybe
import Debug.Trace
import Parser
import Text.Printf

data EstadoExecucao = EstadoExecucao [Regra] Int

-- Subst = Substituição
type Subst = Maybe [(Termo, Termo)]

showSubst :: Subst -> String
showSubst (Just subst) = showSubstVars (Just subst) (varNamesMany $ map fst subst)
showSubst Nothing = "[sem substituições]"

showSubstVar :: Subst -> Termo -> Termo
showSubstVar subst term |  newTerm == term = term
						| otherwise = showSubstVar subst newTerm
						where newTerm = subst' term subst

showSubstVars :: Subst -> [String] -> String
showSubstVars Nothing _ = "falso."
showSubstVars _ [] = "verdadeiro."
showSubstVars subst varNames = intercalate ", " $ map (\v -> v ++ " = " ++ (show (showSubstVar subst (Variavel v)))) varNames

emptySubst :: Subst
emptySubst = Just []

trace' :: String -> a -> a
trace' s x = trace s x

unify :: Termo -> Termo -> Subst
unify (Atomo a) (Atomo b) 	| a == b = Just []
							| otherwise = Nothing
unify (Variavel x) t = Just [(Variavel x, t)]
unify t (Variavel x) = Just [(Variavel x, t)]
unify (Composto f args1) (Composto g args2) 	| f == g = unifyList args1 args2
												| otherwise = Nothing
unify _ _ = Nothing

unifyList :: [Termo] -> [Termo] -> Subst
unifyList (t:ts) (u:us) = joinSubst s (unifyList (substAll' ts s) (substAll' us s)) where s = unify t u
unifyList [] [] = Just []
unifyList _ [] = Nothing
unifyList [] _ = Nothing

unifyGoalAndRule' :: Termo -> Regra -> (Regra, Subst)
unifyGoalAndRule' goal regra@(Regra head _) = (regra, substitution)
											where substitution = unify goal head

unifyGoalAndRule :: Termo -> Regra -> State EstadoExecucao (Regra, Subst)
unifyGoalAndRule goal regra = state $ \e -> (unifyGoalAndRule' goal regra, e)

joinSubst :: Subst -> Subst -> Subst
joinSubst _ Nothing = Nothing
joinSubst Nothing _ = Nothing
joinSubst (Just s) (Just t) = Just $ s ++ t

substAll' :: [Termo] -> Subst -> [Termo]
substAll' [] _ = []
substAll' ts s = map (`subst'` s) ts

subst'' :: Termo -> Subst -> Termo
subst'' t Nothing = t
subst'' t (Just []) = t
subst'' (Variavel x) (Just ((Variavel x', y):ss')) 	| x == x' = y
													| otherwise = subst'' (Variavel x) (Just ss')
subst'' (Composto f a) ss = Composto f (substAll' a ss)
subst'' t _ = t

subst' :: Termo -> Subst -> Termo
subst' t s 	| t == t' = t
			| otherwise = subst' t' s
			where t' = subst'' t s

subst :: Termo -> Subst -> State EstadoExecucao Termo
subst term subst = state $ \e -> (subst' term subst, e)

substAll :: [Termo] -> Subst -> State EstadoExecucao [Termo]
substAll termos st = state $ \e -> (substAll' termos st, e)

varNames :: Termo -> [String]
varNames (Variavel x) = [x]
varNames (Composto f args) = (varNamesMany args)
varNames _ = []

varNamesMany :: [Termo] -> [String]
varNamesMany [] = []
varNamesMany (t:ts) = union (varNames t) (varNamesMany ts)

renomear :: Regra -> State EstadoExecucao Regra
renomear regra@(Regra head body) = do
								rs <- renomearSubst regra
								head' <- subst head rs
								body' <- substAll body rs
								return $ Regra head' body'

renomearMuitos :: [Regra] -> State EstadoExecucao [Regra]
renomearMuitos regras = mapM (renomear) regras

renomearSubst :: Regra -> State EstadoExecucao Subst
renomearSubst (Regra head body) = renomearSubst' (head:body)

renomearSubst' :: [Termo] -> State EstadoExecucao Subst
renomearSubst' termos = renomearSubst'' (varNamesMany termos)

renomearSubst'' :: [String] -> State EstadoExecucao Subst
renomearSubst'' varNames = Just <$> mapM (assinarNovoNome) varNames

assinarNovoNome :: String -> State EstadoExecucao (Termo, Termo)
assinarNovoNome str = state $ \(EstadoExecucao regras cont)
							-> ((Variavel str, Variavel (str ++ "_" ++ (show cont))), (EstadoExecucao regras (cont + 1)))

unifications :: Termo -> [Regra] -> State EstadoExecucao [(Regra, Subst)]
unifications goal regras = mapM (unifyGoalAndRule goal) regras

canUnify :: Termo -> Termo -> Bool
canUnify t1 t2 = (unify t1 t2) /= Nothing

reach'' :: [Termo] -> Subst -> State EstadoExecucao [Subst]
reach'' [] s = state $ \ es -> ([s], es)
reach'' ((Composto "ne" [t1, t2]):gs) s' 	| (not (canUnify t1 t2)) = reach'' gs s'
											| otherwise = return []
reach'' (g:gs) s' = state $ \e@(EstadoExecucao regras _) -> runState (do
																	renomearRegras <- renomearMuitos regras
																	unifs <- unifications g renomearRegras
																	let unifs' = filter (isJust . snd) unifs
																	newSubsts <- mapM id [ trace (printf "`%s' com %s." (show regra) (showSubst s)) (reach' ((substAll' body s) ++ (substAll' gs s)) (joinSubst s' s)) | (regra@(Regra head body), s) <- unifs' ]
																	return $ concat newSubsts) e

reach' gs s = trace' (printf "{%s}" (intercalate ", " $ map show gs)) $ reach'' gs s

reach :: Termo -> State EstadoExecucao [Subst]
reach goal = reach' [goal] emptySubst

reachNew :: Termo -> State EstadoExecucao [Subst]
reachNew goal = state $ \e@(EstadoExecucao regras _) -> runState (reach goal) e