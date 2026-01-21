module Typechecker where
import Text.Parsec (parse)
import Parser (classTableParser)
import Syntax
import qualified Data.Map as Map
import Data.Map (Map)

--  Typing context: maps variable names to their types
type Context = Map Name Type

--  Possible typing errors
data TypeError
  = UnboundVariable Name
  | UnknownField Name Name
  | UnknownMethod Name Name
  | BadMethodArguments Name [Type] [Type]
  | BadConstructorArguments Name [Type] [Type]
  | UnknownClass Name
  deriving (Show, Eq)

--  Main typing function
typeOf :: Context -> ClassTable -> Expr -> Either TypeError Type



--  Check the entire class table before typechecking expressions
checkClassTable :: ClassTable -> Either String ()

checkClassTable ct = do
  checkDuplicateClasses ct
  mapM_ (checkClass ct) ct
  return ()

--  Check for duplicate class names
checkDuplicateClasses :: ClassTable -> Either String ()
checkDuplicateClasses ct =
  if hasDuplicates (map className ct)
    then Left "Duplicate class names found!"
    else Right ()

--  Helper to find duplicates
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

--  Check a single class
checkClass :: ClassTable -> Class -> Either String ()
checkClass ct cls = do
  checkSuperclassExists ct cls
  checkConstructor ct cls
  checkMethodOverriding ct cls
  return ()

--  Check superclass exists
checkSuperclassExists :: ClassTable -> Class -> Either String ()
checkSuperclassExists ct (Class _ super _ _ _)
  | super == "Object" || super == "" = Right ()  
  | otherwise =
      if any (\c -> className c == super) ct
         then Right ()
         else Left $ "Superclass " ++ super ++ " not found."


--  Check constructor matches fields
checkConstructor :: ClassTable -> Class -> Either String ()
checkConstructor _ (Class cname _ fields (Constructor conName conArgs superArgs assignments) _) = do
  if cname /= conName
     then Left $ "Constructor name mismatch in class " ++ cname
     else if map (\(Field ty name) -> (ty, name)) fields /= map (\(Field ty name) -> (ty, name)) conArgs
             then Left $ "Constructor arguments do not match class fields in " ++ cname
             else if map fst assignments /= map snd assignments
                     then Left $ "Constructor assignments are incorrect in class " ++ cname
                     else Right ()

--  Check method overriding is correct
checkMethodOverriding :: ClassTable -> Class -> Either String ()
checkMethodOverriding ct (Class cname super _ _ methods) =
  case lookupClass ct super of
    Nothing -> Right ()  
    Just (Class _ _ _ _ superMethods) -> mapM_ (checkOverride superMethods) methods
  where
    checkOverride :: [Method] -> Method -> Either String ()
    checkOverride superMethods m =
      case lookup (methodName m) [(methodName sm, sm) | sm <- superMethods] of
        Nothing -> Right () 
        Just sm ->
          if methodParams sm == methodParams m && methodReturnType sm == methodReturnType m
             then Right ()
             else Left $ "Method " ++ methodName m ++ " in class " ++ cname ++ " does not correctly override."

--  Lookup a class by name
lookupClass :: ClassTable -> Name -> Maybe Class
lookupClass [] _ = Nothing
lookupClass (c:cs) n
  | className c == n = Just c
  | otherwise = lookupClass cs n



-- (T-Var) Variable typing
typeOf ctx _ (Var x) =
  case Map.lookup x ctx of
    Just ty -> Right ty
    Nothing -> Left (UnboundVariable x)

-- (T-Field) Field Access typing
typeOf ctx ct (FieldAccess e f) = do
  c <- typeOf ctx ct e
  fields <- lookupFields ct c
  case lookup f (map (\(Field ty name) -> (name, ty)) fields) of
    Just ty -> Right ty
    Nothing -> Left (UnknownField c f)

-- (T-Invk) Method Invocation typing
typeOf ctx ct (MethodInvoke e m args) = do
  ce <- typeOf ctx ct e
  (paramTypes, retType) <- lookupMethodType ct ce m
  argTypes <- mapM (typeOf ctx ct) args
  if map fst paramTypes == argTypes
     then Right retType
     else Left (BadMethodArguments m (map fst paramTypes) argTypes)


-- (T-New) Object Creation typing
typeOf ctx ct (New cname args) = do
  fields <- lookupFields ct cname
  argTypes <- mapM (typeOf ctx ct) args
  let fieldTypes = map (\(Field ty _) -> ty) fields
  if argTypes == fieldTypes
     then Right cname
     else Left (BadConstructorArguments cname fieldTypes argTypes)

-- (T-Cast) Casting typing (Upcast, Downcast, Stupid Cast)
typeOf ctx ct (Cast ty e) = do
  eTy <- typeOf ctx ct e
  if isSubtype ct eTy ty || isSubtype ct ty eTy || not (relatedTypes ct eTy ty)
     then Right ty
     else Right ty -- Always allow cast, but runtime might crash


-- Helper functions

--  Lookup all fields of a class, including inherited fields from superclasses.
lookupFields :: ClassTable -> Name -> Either TypeError [Field]
lookupFields [] cname
  | cname == "Object" = Right []  
  | otherwise = Left (UnknownClass cname)
lookupFields (Class n super fields _ _ : rest) cname
  | n == cname =
      if super == ""
         then Right fields
         else do
           superFields <- lookupFields rest super
           Right (superFields ++ fields)
  | otherwise = lookupFields rest cname



-- Lookup method type (param types + return type)
lookupMethodType :: ClassTable -> Name -> Name -> Either TypeError ([(Type, Parameter)], Type)
lookupMethodType [] cname mname = Left (UnknownClass cname)
lookupMethodType (Class n _ _ _ methods : rest) cname mname
  | n == cname =
      case filter (\meth -> methodName meth == mname) methods of
        [meth] -> Right (methodParams meth, methodReturnType meth)
        _ -> Left (UnknownMethod cname mname)
  | otherwise = lookupMethodType rest cname mname

-- Check if two types are related (for stupid casts)
relatedTypes :: ClassTable -> Type -> Type -> Bool
relatedTypes ct c1 c2 = isSubtype ct c1 c2 || isSubtype ct c2 c1

-- Check if first type is a subtype of second type
isSubtype :: ClassTable -> Type -> Type -> Bool
isSubtype _ t1 t2 | t1 == t2 = True
isSubtype [] _ _ = False
isSubtype (Class n super _ _ _ : rest) t1 t2
  | n == t1 = t1 == t2 || isSubtype rest super t2
  | otherwise = isSubtype rest t1 t2





 
objectClass :: Class
objectClass = Class
  { className = "Object"
  , classSuperClass = ""   
  , classFields = []
  , classConstructor = Constructor
      { constructorName = "Object"
      , constructorArgs = []
      , constructorSuperArgs = []
      , constructorAssignments = []
      }
  , classMethods = []
  }


--  High-level function to parse, add Object, check classes, and typecheck
runTypeCheck :: FilePath -> IO ()
runTypeCheck file = do
  content <- readFile file
  case parse classTableParser file content of
    Left err -> putStrLn ("Parse error: " ++ show err)
    Right parsedCT -> do
      let ct = objectClass : parsedCT  
      case checkClassTable ct of
        Left err -> putStrLn ("Class table error: " ++ err)
        Right () -> do
          putStrLn "Class table is OK!"
          putStrLn "Parsed Class Table:"
          print ct
          putStrLn "Typechecking passed!"
