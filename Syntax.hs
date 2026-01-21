module Syntax where

-- Basic types
type Name        = String
type Type        = Name
type Parameter   = Name
type Assignment  = (Name, Name)

-- Class Table
type ClassTable  = [Class]

-- Field name and its type
data Field = Field Type Name
  deriving (Show, Eq)

-- Method declaration
data Method = Method
  { methodReturnType :: Type
  , methodName       :: Name
  , methodParams     :: [(Type, Parameter)] 
  , methodBody       :: Expr
  }
  deriving (Show, Eq)




-- Constructor

data Constructor = Constructor
  { constructorName        :: Name
  , constructorArgs        :: [Field]      
  , constructorSuperArgs   :: [Parameter]  
  , constructorAssignments :: [Assignment]  
  }
  deriving (Show, Eq)


-- Class declaration
data Class = Class
  { className       :: Name
  , classSuperClass :: Name
  , classFields     :: [Field]
  , classConstructor :: Constructor  
  , classMethods    :: [Method]
  }
  deriving (Show, Eq)


-- Expressions
data Expr
  = Var Name
  | FieldAccess Expr Name
  | MethodInvoke Expr Name [Expr]
  | New Name [Expr]
  | Cast Type Expr
  deriving (Show, Eq)
