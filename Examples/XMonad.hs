{-# LANGUAGE Rank2Types #-}
import Language.Haskell.Exts
import System.IO.Unsafe
import Language.Haskell.Exts.Desugar.Generic
import Data.Data
import Data.Set
import Control.Monad

-- loading the AST of StackSet module from XMonad 
stackSet :: Module 
stackSet = unsafePerformIO $ do  
  ParseOk m <- parseFile "Examples/XMonad/StackSet.hs" 
  return m 
  
-- Importing Language.Haskell.Exts.Desugar.Generic introduces for each
-- constructor X in Language.Haskell.Exts.Syntax, a function named
-- desugarX to desugar a term starting with the constructor X. In Order
-- to desugar all terms starting with X in the AST the function genericM
-- desugarX should be used.
  
-- For example, the following code desugars all the list comprehensions in 
-- XMonad's StackSet module. To see the changes, You may compare the
-- file SS_noListComp.hs to SS.hs after executing the following: 
-- ghci> writeFile "SS.hs" (prettyPrint stackSet) 
-- ghci> writeFile "SS_noListComp.hs" (prettyPrint stackSet_no_ListComp)  

stackSet_no_ListComp    = unUnique $ genericM desugarListComp stackSet

-- desugaring Do
-- To compile some of the desugared files, you may need to import the   
--  "hidden" functions manually. For instance, the operator ">>=" is 
-- implicitly imported by the Do notation and you need to import it
-- manually.
stackSet_no_Do          = unUnique $ genericM desugarDo       stackSet

-- Sometimes, it is prefered to compose desugaring functions. For instance,
-- list comprehensions are desugared to Do notations and you may prefer to
-- desugar these Do notations too. However, it is no possible to desugar
-- further than the core constructors (e.g. expression constructors Var,
-- App, Lambda, Case, Let, Lit, Con and RecUpdate)

-- stackset without all the syntactic sugar (expression and pattern)
stackSet_bitter = unUnique $ (    genericM desugarFunBind 
                              >=> genericM desugarListComp      
                              >=> genericM desugarDo_MonoLocalBind 
                              >=> genericM desugarEnumFrom 
                              >=> genericM desugarLeftSection 
                              >=> genericM desugarRightSection 
                              >=> genericM desugarList 
                              >=> genericM desugarIf 
                              >=> genericM desugarTuple  
                              >=> genericM desugarInfixApp 
                              >=> genericM desugarPInfixApp 
                              >=> genericM desugarPList 
                              >=> genericM desugarPTuple 
-- commented because of a bug in the pretty printer
--                              >=> genericM desugarPParen 
--                              >=> genericM desugarParen 
                              ) stackSet
 
-- Too check the constructors in stackSet_bitter, we use the following
-- generic definitions:

-- returning the name of the top-level expression constructors 
gShowConstr :: Data a => a -> String
gShowConstr x = if (typeOf x `elem` 
                    [
                    typeOf (undefined :: Exp)
--                    ,typeOf (undefined :: Pat)
--                    ,typeOf (undefined :: Decl)
                    ]) 
                then showConstr . toConstr $ x
                else ""

-- running the given query function on every node in the input data
everyQ :: Data a => (forall a. Data a => a -> r) -> a -> [r] 
everyQ f x =  f x : (concat $ gmapQ (everyQ f) x)

constructors = delete "" $ fromList $ everyQ gShowConstr stackSet_bitter   