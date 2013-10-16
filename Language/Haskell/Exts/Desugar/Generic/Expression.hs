{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Exts.Desugar.Generic.Expression where

import Prelude hiding (error,foldl,any,foldr)
-- import Prelude              (Integer,Enum(..))

--import Text.Show            (Show(..))

import Language.Haskell.Exts hiding (binds,name)
import Language.Haskell.Exts.SrcLoc(noLoc)
import Language.Haskell.Exts.Unique
-- import Language.Haskell.Exts.Desugar
import Language.Haskell.Exts.Desugar.Basic
-- import Language.Haskell.Exts.Desugar.Type
import Language.Haskell.Exts.Desugar.Generic.Pattern
import Language.Haskell.Exts.Desugar.Generic.Case
import Language.Haskell.Exts.Desugar.Conversion
-- import {-# SOURCE #-} Language.Haskell.Exts.Desugar.Declaration ()
--import Control.Monad        (mapM,sequence,Monad(..),(=<<))

import Control.Applicative  ((<$>),(<*>))

import Data.Foldable        (foldl,foldr,any) -- all,any)
 
-- desugarPTuple = undefined
 

desugarVar, desugarIPVar, desugarCon, desugarLit, desugarInfixApp, desugarApp, desugarNegApp, desugarLambda, desugarLet, desugarIf, desugarCase, desugarDo, desugarMDo, desugarTuple, desugarTupleSection, desugarList, desugarParen, desugarLeftSection, desugarRightSection, desugarRecConstr, desugarRecUpdate, desugarEnumFrom, desugarEnumFromTo, desugarEnumFromThen, desugarEnumFromThenTo, desugarListComp, desugarParComp, desugarExpTypeSig, desugarVarQuote, desugarTypQuote, desugarBracketExp, desugarSpliceExp, desugarQuasiQuote, desugarXTag, desugarXETag, desugarXPcdata, desugarXExpTag, desugarXChildTag, desugarCorePragma, desugarSCCPragma, desugarGenPragma, desugarProc, desugarLeftArrApp, desugarRightArrApp, desugarLeftArrHighApp, desugarRightArrHighApp :: Exp -> Unique Exp

-- Haskell 2010 Report Section 3.2
-- no desugaring
desugarVar = noDesugaring
  
-- not supported
desugarIPVar = notSupported
  
-- not Haskell 2010 Report Section 3.2
desugarCon (Con qn) = return $ Var qn
desugarCon e = noDesugaring e
  
-- not Haskell 2010 Report Section 3.2
-- no desugaring  
-- could be considered as constant functions (Var)
desugarLit = noDesugaring
  
-- Haskell 2010 Report Section  3.4
desugarInfixApp (InfixApp exp1 (QVarOp qName) exp2) = 
    return $ App (App (Var qName) exp1) exp2
desugarInfixApp (InfixApp exp1 (QConOp qName) exp2) = 
    return $ App (App (Con qName) exp1) exp2    
desugarInfixApp e = noDesugaring e  

-- Haskell 2010 Report Section 3.2
-- no desugaring
desugarApp = noDesugaring
  
-- Haskell 2010 Report Section 3.4  
desugarNegApp (NegApp e) =
   return $  App (Var (UnQual (Ident "negate"))) e
desugarNegApp e = noDesugaring e   

-- Haskell 2010 Report Section 3.3   
-- HSE Specific Desugaring
desugarLambda (Lambda _ [] _) = 
  error "No header for the lambda expression!"
desugarLambda (Lambda src (p:[]) body) 
  | not $ isPVar p = do
    name <- newVar
    return $ Lambda src [PVar $ Ident name] 
      (Case (Var $ UnQual $ Ident name) 
       [Alt noLoc p (UnGuardedAlt body) emptyBind])
desugarLambda e@(Lambda _src (_p:[]) _body) 
  | otherwise     = 
    noDesugaring e                       
desugarLambda (Lambda src ps@(_p:_pss) body) 
  | any (not . isPVar) ps = do
    names <- sequence [newVar| _ <-ps]
    desugarLambda $
      Lambda src ((PVar . Ident) <$> names) 
      (Case (Tuple $ (Var . UnQual . Ident) <$> names) 
       [Alt noLoc (PTuple ps) (UnGuardedAlt body) emptyBind])
desugarLambda (Lambda src (p:ps) body) 
  | otherwise = 
    Lambda src [p] <$> desugarLambda (Lambda src ps body)
desugarLambda e = noDesugaring e    

-- Haskell 2010 Report Section 3.12
-- let with empty binding is removed
desugarLet (Let (BDecls []) ex) = return ex 
desugarLet e@(Let _ _)          = noDesugaring e
desugarLet e                    = noDesugaring e  

-- Haskell 2010 Report Section 3.6
desugarIf (If cond th el) = return $  
    Case cond 
    [Alt noLoc 
      (PApp (UnQual (Ident "True")) []) (UnGuardedAlt th) emptyBind
    ,Alt noLoc 
     (PApp (UnQual (Ident "False")) []) (UnGuardedAlt el) emptyBind] 
desugarIf e = noDesugaring e    
  
       
{-

Case con []                                            error
Case con (_                              :[]         ) stepA
Case con (A   _   UG                   eb: emptAlt:[]) stepA
Case con (A  con (G []               ) eb: emptAlt:[]) error
Case con (A  con (G  [GAlt _ []    _]) eb: emptAlt:[]) error 
Case con (A  con (G  [GAlt _ [Q]   _]) eb: emptAlt:[]) stepV
Case con (A  con (G  [GAlt _ [L]   _]) eb: emptAlt:[]) stepU
Case con (A  con (G  [GAlt _ [G]   _]) eb: emptAlt:[]) stepT
Case con (A  con (G  [GAlt _ (_:_) _]) eb: emptAlt:[]) stepS 
Case con (A  con (G  (_:_)           ) eb: emptAlt:[]) stepA 
Case con (A ~con (G _                ) eb: emptAlt:[]) stepA
Case con (_                              : _      :[]) stepA 
Case con (_                              : _      : _) stepA 

Case var []                                            error
Case var (_                              :[]         ) stepJ'
Case var (A pvar         ug            eb: emptAlt:[]) stepIJ
Case var (A PIrrPat      ug            eb: emptAlt:[]) stepD
Case var (A PAsPat       ug            eb: emptAlt:[]) stepE
Case var (A PWildCard    ug            eb: emptAlt:[]) stepF
Case var (A PLit         ug            eb: emptAlt:[]) stepH 
Case var (A PBangPat     ug            eb: emptAlt:[]) stepT'
Case var (A PViewPat     ug            eb: emptAlt:[]) stepV'
Case var (A PNPlusK      ug            eb: emptAlt:[]) stepS'
Case var (A PApp(isvar)  ug            eb: emptAlt:[]) stepFinal 
Case var (A pApp(notvar) ug            eb: emptAlt:[]) stepG
Case var (A _            ug            eb: emptAlt:[]) error
Case var (A _            ug             _: emptAlt:[]) stepBinding 


Case var (A p            g          decls: emptAlt:[]) stepC
 

Case var (_                              :      _ :[]) stepB
Case var (_                              :      _ :_ ) stepB

 
-}

-- Haskell 2010 Report Sections 3.13 & 3.17.3   
-- the optimization transformations (k .. r) are not implemented
desugarCase (Case e altsi) = do 
    -- reduce patterns to:
    -- PApp, PVar, PLit, PAsPat, PWildCard, PIrrPat,   
    -- PNPlusk, PBangPat, PViewPat, PRecord   
    alts <- sequence $
            [(Alt srcLoc) <$> desugarPat  pat 
             <*> return guardedAlts
             <*> return binds 
            |Alt srcLoc pat guardedAlts binds <- altsi]
    let c = Case e alts      
    case e of 
      {Con (Special UnitCon) -> 
          case alts of    
            {[]   -> error "Wrong HSE Tree!"
            ;_:[] -> stepA c       
            ;Alt _ p galt _ : Alt _ PWildCard (UnGuardedAlt _)(BDecls []):[]-> 
                case galt of
                  {UnGuardedAlt _  -> stepA c
                  ;GuardedAlts gss -> 
                    case p of
                      PApp (Special UnitCon) [] -> 
                          case gss of
                            [] -> error "Wrong HSE Tree!"
                            GuardedAlt _ stmts _:[] -> 
                              case stmts of 
                                []                -> error "Wrong HSE Tree!"
                                [Qualifier _]     -> desugarCase =<< _stepV c
                                [LetStmt _]       -> stepU c
                                [Generator _ _ _] -> desugarCase =<< stepT c
                                _:_               -> desugarCase =<< stepS c 
                            _:_ -> stepA c 
                      _ -> stepA c 
                  }
            ;_:_:[] -> stepA c 
            ;_:_:_  -> stepA c}
      ;Var _ -> 
          case alts of
            {[]   -> error "Wrong HSE Tree!"
            ;_:[] -> desugarCase =<< stepJ' c
            ;Alt _ p galt decls:Alt _ PWildCard (UnGuardedAlt _)(BDecls []):[]->
              case galt of
                {UnGuardedAlt _ -> 
                    case decls of
                      {BDecls [] -> 
                          case p of
                            {PVar    _       -> stepIJ                  c
                            ;PIrrPat _       -> desugarCase =<<  stepD  c
                            ;PAsPat _ _      -> desugarCase =<<  stepE  c
                            ;PWildCard       -> stepF                   c
                            ;PLit _          -> desugarCase =<< _stepH  c
                            ;PBangPat _      -> desugarCase =<<  stepT' c
                            ;PViewPat _ _    -> desugarCase =<<  stepV' c
                            ;PNPlusK _ _     -> desugarCase =<<  stepS' c
                            ;PRec _ []       -> return                  c
                            ;PRec _ [_]      -> stepN                   c 
                            ;PRec _ (_:_)    -> stepM                   c
                            ;PApp _ ps
                             | all isPVar ps -> return                  c
                             | True          -> desugarCase =<< stepG   c
                            ;_ -> error "Not Supported in case!"}
                      ;_ -> desugarCase =<< stepBinding c} 
                ;GuardedAlts _ -> desugarCase =<< stepC c}
            ;_:_:[] -> desugarCase =<< stepB c
            ;_:_:_  -> desugarCase =<< stepB c}
      ; _ -> stepA c}
desugarCase e = noDesugaring e      
 
-- Haskell 2010 Report Section 3.14
desugarDo = _desugarDo False

desugarDo_MonoLocalBind ::  Desugaring Exp
desugarDo_MonoLocalBind = _desugarDo True

_desugarDo :: Bool -> Desugaring Exp
_desugarDo _ (Do [])                 = error "Empty do block!"
_desugarDo _ (Do ((Qualifier e):[])) = return e
_desugarDo _ (Do ( _           :[])) = 
  error "The last statement in a 'do' block must be an expression!"
_desugarDo _ (Do ((Qualifier e):ss)) = do
  return $ InfixApp e (QVarOp (UnQual (Symbol ">>"))) (Do ss)    
_desugarDo False (Do ((Generator _ p e):stmts)) = do
  ok <- newVar
  return $ Let 
    (BDecls 
     [FunBind 
      [Match noLoc (Ident ok) 
       [p] Nothing 
       (UnGuardedRhs (Do stmts)) 
       emptyBind
      ,Match noLoc (Ident ok) 
       [PWildCard] Nothing 
       (UnGuardedRhs (App (Var (UnQual (Ident "fail"))) 
                      (Lit (String "...")))) 
       emptyBind]]) 
    (InfixApp e 
     (QVarOp (UnQual (Symbol ">>="))) 
     (Var (UnQual (Ident ok))))

_desugarDo True (Do ((Generator _ (PVar n) e):stmts)) = do
  return $ InfixApp e 
    (QVarOp (UnQual (Symbol ">>="))) 
    (Lambda noLoc [PVar n] (Do stmts)) 
      
_desugarDo True (Do ((Generator _ p e):stmts)) = do
  x <- newVar
  return $ InfixApp e (QVarOp (UnQual (Symbol ">>="))) 
    (Lambda noLoc [PVar $ Ident x] 
     (Case (Var $ UnQual $ Ident x) 
      [Alt noLoc p         (UnGuardedAlt (Do stmts)) 
       emptyBind
      ,Alt noLoc PWildCard (UnGuardedAlt (App (Var (UnQual (Ident "fail"))) 
                                          (Lit (String "...")))) 
       emptyBind]))
  
_desugarDo _ (Do ((LetStmt bs):ss)) =   
  return $ Let bs (Do ss)
_desugarDo _ (Do ((RecStmt _) : (_ : _))) =
  error "not supported yet!" --ToDo 
_desugarDo _ e = noDesugaring e

desugarMDo = notSupported

-- not Haskell 2010 Report Section 3.8 
-- HSE
desugarTuple (Tuple [])         = return $ Con (Special UnitCon)
desugarTuple (Tuple [e])        = return e
desugarTuple (Tuple exps@(_:_)) = return $ 
  foldl App (Con $ Special $ TupleCon Boxed $ length exps) exps
desugarTuple e = noDesugaring e

-- HSE
desugarTupleSection (TupleSection mes) = do 
    eExps <- mapM (\m -> case m of {Nothing -> Left . Ident <$> newVar
                                   ;Just x  -> return $ Right x  }
                   ) mes
    return $ Lambda noLoc [PVar n | Left n  <- eExps] 
      (Tuple [case x of   
                 Left n  -> Var $ UnQual n
                 Right e -> e 
             |x <- eExps])
desugarTupleSection e = noDesugaring e       
        

-- Haskell 2010 Report Section 3.7
desugarList (List es) = return $
    foldr (\e a -> App (App (Con $ Special Cons) e) a)
                  (Con (Special ListCon)) es
desugarList e           = noDesugaring e

-- Haskell 2010 Report Section 3.9
desugarParen (Paren ex) = return ex
desugarParen e          = noDesugaring e

-- Haskell 2010 Report Section 3.5     
desugarLeftSection (LeftSection ex qOp) = do 
  n <- newVar
  return $ Lambda noLoc [PVar $ Ident n] 
    (InfixApp ex qOp (Var $ UnQual $ Ident n))
desugarLeftSection e = noDesugaring e   

-- Haskell 2010 Report Section 3.5
desugarRightSection (RightSection qOp ex) = do
  n <- newVar
  return $ Lambda noLoc [PVar $ Ident n] 
    (InfixApp (Var $ UnQual $ Ident n) qOp ex )
desugarRightSection e = noDesugaring e    
  
-- not Haskell 2010 Report Section 3.15.2
desugarRecConstr = noDesugaring
{-  
  (RecConstr qName fieldUpdates) = do
   qn <- addPrefixToQName newPrefix qName
   fu <- desugarFieldUpdates fieldUpdates  
   desugarRecordUpdate $ RecUpdate (Var qn) fu
-}
  
-- not Haskell 2010 Report Section 3.15.3
desugarRecUpdate = noDesugaring
{-  
  (RecUpdate eexp fieldUpdates) = do 
    fu <- desugarFieldUpdates fieldUpdates
    es <- sequence  
          [do  
           {qn <- addPrefixToQName setPrefix  qName
           ;return $ App (Var qn) e}
          | FieldUpdate qName e  <- fu]
    return $ foldl (flip App) eexp es
-}
  
-- Haskell 2010 Report Section 3.10
desugarEnumFrom (EnumFrom e) =
  return $ App (Var (UnQual (Ident "enumFrom"))) e
desugarEnumFrom e = noDesugaring e    
  
-- Haskell 2010 Report Section 3.10
desugarEnumFromTo  (EnumFromTo ex1 ex2) = do
  return $ App (App (Var (UnQual (Ident "enumFromTo"))) ex1) ex2      
desugarEnumFromTo e = noDesugaring e    

-- Haskell 2010 Report Section 3.10
desugarEnumFromThen  (EnumFromThen ex1 ex2) = 
  return $ App (App (Var (UnQual (Ident "enumFromThen"))) ex1) ex2
desugarEnumFromThen e = noDesugaring e  

-- Haskell 2010 Report Section 3.10
desugarEnumFromThenTo   (EnumFromThenTo ex1 ex2 ex3) =
  return $  
  App (App (App (Var (UnQual (Ident "enumFromThen"))) ex1) ex2) ex3
desugarEnumFromThenTo e = noDesugaring e  

-- not Haskell 2010 Report Section 3.11
-- Generalized to monad comprehensions  
-- based on: "Comprehending Moands" By Walder   
desugarListComp (ListComp eexp qualStmts) =
  return $ 
  Do ( (cQualStmt <$> qualStmts) ++ 
       [(Qualifier (App (Var ((UnQual (Ident "return")))) eexp))])         
desugarListComp e = noDesugaring e
  
-- Not Supported 
desugarParComp = notSupported
 {- 
  desugar (ParComp  eexp qualStmtss) =  error "Not supported!"
  let comps = [ ListComp 
                (Tuple [patToExp p| (Generator _ p _ ) <- qualStmts ]) 
                qualStmts 
              |  qualStmts <- qualStmtss
  let x = Generator noLoc (PTuple [PTuple  | <-]) 
      (foldl App (Var (UnQual $ Ident ("zip" ++ (show (length comps))))) comps )
       desugar (ListComp eexp x)  -}  

-- Haskell 2010 Report Section 3.16
desugarExpTypeSig (ExpTypeSig _ e t) = do
  v <- newVar 
  return $ Let (BDecls [TypeSig noLoc [Ident v] t
                       ,PatBind noLoc (PVar (Ident v)) Nothing
                        (UnGuardedRhs e) emptyBind]) 
    (Var (UnQual (Ident v)))
desugarExpTypeSig e = noDesugaring e    

  
-- Template Haskell
desugarVarQuote        = notSupported
desugarTypQuote        = notSupported      
desugarBracketExp      = notSupported       
desugarSpliceExp       = notSupported      
desugarQuasiQuote      = notSupported       
-- Hsx
desugarXTag            = notSupported      
desugarXETag           = notSupported      
desugarXPcdata         = notSupported      
desugarXExpTag         = notSupported      
desugarXChildTag       = notSupported     
-- Pragmas
desugarCorePragma      = notSupported     
desugarSCCPragma       = notSupported    
desugarGenPragma       = notSupported   
-- Arrows
desugarProc            = notSupported      
desugarLeftArrApp      = notSupported      
desugarRightArrApp     = notSupported   
desugarLeftArrHighApp  = notSupported 
desugarRightArrHighApp = notSupported


stepFinal :: Exp -> Unique Exp
stepFinal c@(Case (Var _) 
           ((Alt _ (PApp _ ps) (UnGuardedAlt _) 
             (BDecls [])) 
            :(Alt _ PWildCard (UnGuardedAlt _) 
              (BDecls []))
            :[])) 
  | all isPVar ps
  = noDesugaring c  
stepFinal _ = error "not in the final state!"     

{-   
desugarFieldUpdate :: FieldUpdate -> Unique FieldUpdate  
desugarFieldUpdate (FieldUpdate qn e) =  
  return $ FieldUpdate qn e
desugarFieldUpdate (FieldPun n)       = 
  desugarFieldUpdate $ FieldUpdate (UnQual n) (Var (UnQual n))
desugarFieldUpdate FieldWildcard      = 
  error "FieldWildcard is not supported!"
  -}  
 

  
  