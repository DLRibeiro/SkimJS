module Value (Value (..)) where
import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
	  | GlobalVar
	  | Double Double
	  | Break (Maybe Id)
	  | Function Id [Id] [Statement]
	  | Return Value
	  | Array [Value]
	  | Empty
	deriving (Eq)
	

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show (Function id [args] [comands] ) = "Func: " ++ id ++ (showArgs args)
  show (Return v) = show v
  show (Array b) = "[" ++ (showArray (Array b)) ++ "]"
  show GlobalVar = "Undefined variable"
  show (Double d) = show d
  
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showArgs [] = ""
showArgs (Id c :cs) = (show c) ++ ";" ++ showArgs cs

showArray [] = "";
showArray [b] = show b
showArray (Array (b:bs)) = show b ++ "," showArray bs

showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
