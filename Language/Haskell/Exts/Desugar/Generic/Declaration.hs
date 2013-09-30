{-# LANGUAGE FlexibleInstances #-} 
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

--NOT Supported: 
-- Arrows, Implicit Parameters, Template Haskell, Scoped Type Variables
-- XML,Regular, Generic Style, Mdo, Pragmas, Module,TransformListComp
-- FFI, GADTs, TypeFamilies, Fundeps, Datatype Contexts 

module Language.Haskell.Exts.Desugar.Generic.Declaration where

import qualified Prelude 
import Prelude              (Integer,Enum(..),Eq(..))

import Language.Haskell.Exts hiding (name)
import Language.Haskell.Exts.SrcLoc(noLoc)
import Language.Haskell.Exts.Unique
import Language.Haskell.Exts.Desugar.Basic
-- import Language.Haskell.Exts.Desugar.Type ()
import Language.Haskell.Exts.Desugar.Generic.Pattern 
import Language.Haskell.Exts.Desugar.Conversion 
import Language.Haskell.Exts.Desugar.Generic.Expression ()
import Control.Monad        (mapM,sequence,Monad(..))

import Control.Applicative  ((<$>))

import Data.Function        (($),(.))
import Data.Maybe           (Maybe(..))
import Data.List            ((++),length,concat)

desugarTypeDecl, desugarTypeFamDecl, desugarDataDecl, desugarGDataDecl
 , desugarDataFamDecl, desugarTypeInsDecl, desugarDataInsDecl 
 , desugarGDataInsDecl, desugarClassDecl, desugarInstDecl, desugarDerivDecl
 , desugarInfixDecl, desugarDefaultDecl, desugarSpliceDecl, desugarTypeSig
 , desugarFunBind , desugarPatBind , desugarForImp , desugarForExp
 , desugarRulePragmaDecl , desugarDeprPragmaDecl , desugarWarnPragmaDecl
 , desugarInlineSig , desugarInlineConlikeSig , desugarSpecSig
 , desugarSpecInlineSig , desugarInstSig , desugarAnnPragma :: Desugaring Decl

desugarTypeDecl  = notSupported    
desugarDataDecl  = notSupported
desugarDerivDecl = notSupported
desugarClassDecl = notSupported
desugarInstDecl  = noDesugaring  
desugarTypeSig   = noDesugaring
      
-- 4.4.3.1
desugarFunBind (FunBind ms@((Match _ n ps _ _ _ ):_)) =  do
  names <- sequence [ Ident <$> newVar 
                    | _  <- [1..length ps]]
  return $ PatBind noLoc (PVar n) Nothing 
    (UnGuardedRhs (Lambda noLoc (PVar <$> names) 
                   (Case (Tuple $ (Var . UnQual) <$> names) 
                    (cMatchs ms))))
    emptyBind
desugarFunBind e = noDesugaring e              
                   
desugarPatBinds :: Desugaring [Decl]                   
desugarPatBinds ds =  concat <$> (mapM _desugarPatBind ds)
    
desugarPatBind _ = error $ "Desugaring a PatBind results in multiple"
  ++" declarations; instead, use desugarPatBinds or _desugarPatBind"

-- Haskell 2010 Report Section  4.4.3.2
_desugarPatBind :: Decl -> Unique [Decl]
_desugarPatBind e@(PatBind _src(PVar (Ident _)) _m (UnGuardedRhs _)(BDecls [])) =
  noDesugaring [e] -- Final State
_desugarPatBind (PatBind src p m (GuardedRhss  grhss) bnds) =
  _desugarPatBind $
   PatBind src p m 
   (UnGuardedRhs 
    (Let bnds 
     (Case (Con (Special UnitCon)) 
      [Alt noLoc (PApp (Special UnitCon) []) 
       (GuardedAlts 
        (cGuardedRhs <$> grhss)) emptyBind]))) emptyBind         
_desugarPatBind (PatBind _src p mt (UnGuardedRhs e) (BDecls [])) = do
  -- Typing Haskell in Haskell Section 11.6.3 
  seed <- newVar
  concat <$> mapM _desugarPatBind   
    ((PatBind noLoc (PVar $ Ident $ seed) mt (UnGuardedRhs e) emptyBind)
     :
     ((\v ->
        (PatBind noLoc (PVar $ v) Nothing 
         (UnGuardedRhs 
          (Case (Var (UnQual (Ident $ seed))) 
           [Alt noLoc p (UnGuardedAlt (Var (UnQual v))) 
            emptyBind])) emptyBind)) <$> (patVar p) 
     ))
_desugarPatBind (PatBind src p mt (UnGuardedRhs e) bnds) = _desugarPatBind $
  PatBind src p mt (UnGuardedRhs (Let bnds e)) emptyBind     
_desugarPatBind e = noDesugaring [e]  

desugarTypeFamDecl      = notSupported
desugarGDataDecl        = notSupported
desugarDataFamDecl      = notSupported
desugarDataInsDecl      = notSupported
desugarTypeInsDecl      = notSupported
desugarGDataInsDecl     = notSupported
desugarDefaultDecl      = notSupported 
desugarSpliceDecl       = notSupported
desugarInfixDecl        = notSupported
desugarForImp           = notSupported
desugarForExp           = notSupported
desugarRulePragmaDecl   = notSupported
desugarDeprPragmaDecl   = notSupported
desugarWarnPragmaDecl   = notSupported
desugarInlineSig        = notSupported
desugarInlineConlikeSig = notSupported
desugarSpecSig          = notSupported
desugarSpecInlineSig    = notSupported
desugarInstSig          = notSupported
desugarAnnPragma        = notSupported

 

  