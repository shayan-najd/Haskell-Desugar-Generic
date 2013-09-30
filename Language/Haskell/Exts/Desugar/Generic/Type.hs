{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Exts.Desugar.Generic.Type where

import Prelude (return,($),foldl,length)
import Language.Haskell.Exts
import Language.Haskell.Exts.Unique

desugarKindStar, desugarKindBang, desugarKindFn, desugarKindVar
 , desugarKindParen :: Desugaring Kind

desugarKindStar = noDesugaring 
 
desugarKindBang = noDesugaring

desugarKindFn   = noDesugaring

desugarKindVar  = noDesugaring  

desugarKindParen (KindParen k) = return k
desugarKindParen e             = noDesugaring e 
    
                                 
desugarClassA, desugarEqualP, desugarInfixA, desugarIParam :: Desugaring Asst

desugarClassA = noDesugaring
  
desugarEqualP = noDesugaring
 
desugarInfixA (InfixA t1 qName t2) = return $ 
  ClassA qName [t1,t2]    
desugarInfixA e = noDesugaring e
  
desugarIParam = notSupported
  

desugarKindedVar, desugarUnkindedVar :: Desugaring TyVarBind

desugarKindedVar   = noDesugaring

desugarUnkindedVar = noDesugaring   
 
    
desugarTyForall, desugarTyFun, desugarTyTuple, desugarTyList, desugarTyApp
 , desugarTyVar, desugarTyCon, desugarTyParen, desugarTyInfix
 , desugarTyKind :: Desugaring Type
 
desugarTyForall = noDesugaring 
 
desugarTyFun (TyFun t1 t2) =  return $ 
  TyInfix t1 (Special FunCon) t2
desugarTyFun e = noDesugaring e  

desugarTyTuple (TyTuple b ts) = return $
  foldl TyApp (TyCon $ Special $ TupleCon b $ length ts) ts
desugarTyTuple e = noDesugaring e 

desugarTyList (TyList  t) = return $
  TyApp (TyCon $ Special ListCon) t 
desugarTyList e = noDesugaring e   

desugarTyApp = noDesugaring

desugarTyVar = noDesugaring

desugarTyCon = noDesugaring

desugarTyParen (TyParen t) = return t      
desugarTyParen e = noDesugaring e   

desugarTyInfix (TyInfix t1 qn t2) = return $
  TyApp (TyApp (TyCon qn) t1) t2  
desugarTyInfix e = noDesugaring e   
  
desugarTyKind = noDesugaring
 
{-
tyvars :: Type -> [Name]
--tyvars (TyForall Nothing ctx t) = tyvars t --ambiguous types are not supported
tyvars (TyApp t1 t2) = (tyvars t1) `union`  (tyvars t2) 
tyvars (TyVar n) = [n]  
tyvars (TyCon _) = []                 
tyvars _ = Prelude.error "Not supported!"
-}    