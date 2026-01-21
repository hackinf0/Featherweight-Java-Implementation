module Evaluator where

import Syntax
import Typechecker
import qualified Data.Map as Map


step :: ClassTable -> Expr -> Maybe Expr
step ct expr = case expr of
  -- (E-Field) 
  FieldAccess (New cname args) fieldName -> do
    index <- lookupFieldIndex ct cname fieldName
    if index < length args
       then Just (args !! index)
       else Nothing

  -- (E-Field inner reduction)
  FieldAccess e f -> do
    e' <- step ct e
    Just (FieldAccess e' f)

  -- (E-New-Args)
  New cname args -> do
    args' <- reduceArgs ct args
    Just (New cname args')

  -- (E-Invk)
  MethodInvoke e m es -> 
    case e of
      New cname args -> do
        (params, body) <- lookupMethodBody ct cname m
        if length params == length es
           then do
             let subst = Map.fromList (zip (map snd params) es ++ [("this", New cname args)])
             Just (substitute subst body)
           else Nothing
      _ -> do
        e' <- step ct e
        Just (MethodInvoke e' m es)

  -- (E-Cast inner reduction)
  Cast ty e -> do
    e' <- step ct e
    Just (Cast ty e')

  -- No rule applies
  _ -> Nothing



--  Try to reduce one argument inside a list
reduceArgs :: ClassTable -> [Expr] -> Maybe [Expr]
reduceArgs _ [] = Nothing
reduceArgs ct (e:es) =
  case step ct e of
    Just e' -> Just (e' : es)
    Nothing -> fmap (e :) (reduceArgs ct es)



--  Lookup the index of a field inside a class (including superclasses)
lookupFieldIndex :: ClassTable -> Name -> Name -> Maybe Int
lookupFieldIndex [] _ _ = Nothing
lookupFieldIndex (Class n super fields _ _ : rest) cname fname
  | n == cname = lookupIndex fields fname
  | otherwise  = lookupFieldIndex rest cname fname

--  Lookup field index in a list of fields
lookupIndex :: [Field] -> Name -> Maybe Int
lookupIndex fields fname = lookupIndex' fields fname 0
  where
    lookupIndex' [] _ _ = Nothing
    lookupIndex' (Field _ name : rest) fname i
      | name == fname = Just i
      | otherwise     = lookupIndex' rest fname (i + 1)



--  Find method body (params and body) in class table
lookupMethodBody :: ClassTable -> Name -> Name -> Maybe ([(Type, Parameter)], Expr)
lookupMethodBody [] _ _ = Nothing
lookupMethodBody (Class n _ _ _ methods : rest) cname mname
  | n == cname =
      case filter (\meth -> methodName meth == mname) methods of
        [meth] -> Just (methodParams meth, methodBody meth)
        _ -> Nothing
  | otherwise = lookupMethodBody rest cname mname


--  Substitute variables inside an expression
substitute :: Map.Map Name Expr -> Expr -> Expr
substitute env expr = case expr of
  Var x -> Map.findWithDefault (Var x) x env
  FieldAccess e f -> FieldAccess (substitute env e) f
  MethodInvoke e m es -> MethodInvoke (substitute env e) m (map (substitute env) es)
  New cname args -> New cname (map (substitute env) args)
  Cast ty e -> Cast ty (substitute env e)

