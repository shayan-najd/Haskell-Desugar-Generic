{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Exts.Desugar.Generic.Pattern where

import qualified Prelude
import Language.Haskell.Exts (Pat(..),Name(..),QName(..),Literal(..),PatField(..)
                             ,SpecialCon(..),Boxed(..),prettyPrint)  
import Language.Haskell.Exts.Unique
import Prelude (($),(++),return,length,foldr,concat,Int)
import Control.Applicative ((<$>))
import Control.Monad ((>=>))

desugarPVar, desugarPLit, desugarPNeg, desugarPNPlusK, desugarPInfixApp
 , desugarPApp, desugarPTuple, desugarPList, desugarPParen, desugarPRec
 , desugarPAsPat, desugarPWildCard, desugarPIrrPat, desugarPatTypeSig
 , desugarPViewPat, desugarPRPat, desugarPXTag, desugarPXETag
 , desugarPXPcdata, desugarPXPatTag, desugarPXRPats, desugarPExplTypeArg
 , desugarPQuasiQuote, desugarPBangPat, desugarPat   :: Desugaring Pat

desugarPat = desugarPNeg >=> desugarPNPlusK >=> desugarPInfixApp 
         >=> desugarPTuple >=> desugarPList >=> desugarPParen 
         >=> desugarPBangPat

desugarPVar      = noDesugaring

desugarPLit      = noDesugaring

-- should be removed from HSE
-- and replaced with literals
desugarPNeg (PNeg (PLit (Int x)))  = return $ PLit (Int (-x))
desugarPNeg (PNeg (PLit (Frac y))) = return $ PLit (Frac (-y))
desugarPNeg (PNeg _other)          = 
   error $ "In Patterns, negation can only "
   ++      "be applied to numeric literals!"
desugarPNeg e = noDesugaring e   

desugarPNPlusK = noDesugaring

desugarPInfixApp (PInfixApp pat1 qName pat2) = return $
   PApp qName [pat1,pat2]
desugarPInfixApp e = noDesugaring e

desugarPApp      = noDesugaring

-- HSE
desugarPTuple (PTuple [])   = return $ 
  PApp (Special UnitCon) []
desugarPTuple (PTuple [p])  = return p  
desugarPTuple (PTuple pats) = 
  return $ PApp (Special $ TupleCon Boxed $ length pats) pats
desugarPTuple e = noDesugaring e 

-- HSE
desugarPList (PList []) = return $ PApp (Special ListCon) []
desugarPList (PList ps) = return $ 
   foldr (\p a -> PApp (Special Cons) [p,a]) (PList [])  ps
desugarPList e = noDesugaring e  

desugarPParen (PParen pat) = return pat
desugarPParen e = noDesugaring e

desugarPRec         = noDesugaring

desugarPAsPat       = noDesugaring

desugarPWildCard    = noDesugaring

desugarPIrrPat      = noDesugaring

desugarPatTypeSig   = notSupported 

desugarPViewPat     = noDesugaring

desugarPRPat        = notSupported    
 
desugarPXTag        = notSupported    

desugarPXETag       = notSupported    

desugarPXPcdata     = notSupported     

desugarPXPatTag     = notSupported      

desugarPXRPats      = notSupported

desugarPExplTypeArg = notSupported 

desugarPQuasiQuote  = notSupported 

desugarPBangPat     = noDesugaring
   
desugarPFieldPat, desugarPFieldPun, desugarPFieldWildcard
 , desugarPatField :: Desugaring PatField
desugarPatField =  desugarPFieldPun

desugarPFieldPat = noDesugaring

desugarPFieldPun (PFieldPun n)  = return $ PFieldPat (UnQual n) (PVar n)
desugarPFieldPun e              = noDesugaring e

desugarPFieldWildcard = notSupported

{-
instance Desugar PatField where
  desugar (PFieldPat qName p) = 
    PFieldPat $$ qName ** p
  desugar (PFieldPun n)       =  
    desugar $
  desugar PFieldWildcard      =  
    error "PFieldWildcard is not supported!"
-}

-- variables / name bound in a pattern
patVar :: Pat -> [Name]      
patVar (PVar name)  = [name]  
patVar (PLit _) = []                   
patVar (PatTypeSig _ pat _) = patVar pat  
patVar (PApp _ pats) = concat (patVar <$> pats)
patVar (PAsPat name pat) = name : (patVar pat)  
patVar (PParen pat) = patVar pat  
patVar (PIrrPat pat) = patVar pat 
patVar (PBangPat pat) = patVar pat 
patVar (PNeg _) = [] 
patVar (PInfixApp pat1 _ pat2) = (patVar pat1) ++ (patVar pat2)
patVar (PTuple pats) =  concat $ patVar <$> pats    
patVar (PList pats) = concat $ patVar <$> pats  
patVar PWildCard  = []  
patVar (PNPlusK _ _) = []
patVar (PViewPat _ pat) = patVar pat  
patVar x = Prelude.error $ "Pattern " ++ (prettyPrint x) ++ " is not supported!"