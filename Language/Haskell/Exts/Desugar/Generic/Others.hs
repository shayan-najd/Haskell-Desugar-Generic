{-# LANGUAGE FlexibleInstances #-} 
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
 
module Language.Haskell.Exts.Desugar.Generic.Others where

import qualified Prelude 
import Language.Haskell.Exts 
import Language.Haskell.Exts.Unique
 
desugarModuleName :: Desugaring ModuleName
desugarModuleName = noDesugaring

desugarUnitCon, desugarListCon, desugarFunCon, desugarTupleCon, desugarCons, desugarUnboxedSingleCon :: Desugaring SpecialCon
desugarUnitCon = noDesugaring
desugarListCon = noDesugaring
desugarFunCon = noDesugaring
desugarTupleCon = noDesugaring
desugarCons = noDesugaring
desugarUnboxedSingleCon = noDesugaring

desugarQual, desugarUnQual, desugarSpecial :: Desugaring QName
desugarQual = noDesugaring
desugarUnQual = noDesugaring
desugarSpecial = noDesugaring

desugarIdent, desugarSymbol :: Desugaring Name
desugarIdent = noDesugaring
desugarSymbol = noDesugaring

desugarIPDup, desugarIPLin :: Desugaring IPName
desugarIPDup = noDesugaring
desugarIPLin = noDesugaring

desugarQVarOp, desugarQConOp :: Desugaring QOp
desugarQVarOp = noDesugaring
desugarQConOp = noDesugaring

desugarVarOp, desugarConOp :: Desugaring Op
desugarVarOp = noDesugaring
desugarConOp = noDesugaring

desugarVarName, desugarConName :: Desugaring CName
desugarVarName = noDesugaring
desugarConName = noDesugaring

desugarModule :: Desugaring Module
desugarModule = noDesugaring

desugarEVar, desugarEAbs, desugarEThingAll, desugarEThingWith, desugarEModuleContents :: Desugaring ExportSpec
desugarEVar = noDesugaring
desugarEAbs = noDesugaring
desugarEThingAll = noDesugaring
desugarEThingWith = noDesugaring
desugarEModuleContents = noDesugaring

desugarIVar, desugarIAbs, desugarIThingAll, desugarIThingWith :: Desugaring ImportSpec
desugarIVar = noDesugaring
desugarIAbs = noDesugaring
desugarIThingAll = noDesugaring
desugarIThingWith = noDesugaring

desugarAssocNone, desugarAssocLeft, desugarAssocRight :: Desugaring Assoc
desugarAssocNone = noDesugaring
desugarAssocLeft = noDesugaring
desugarAssocRight = noDesugaring

desugarAnn, desugarTypeAnn, desugarModuleAnn :: Desugaring Annotation
desugarAnn = noDesugaring
desugarTypeAnn = noDesugaring
desugarModuleAnn = noDesugaring

desugarDataType, desugarNewType :: Desugaring DataOrNew
desugarDataType = noDesugaring
desugarNewType = noDesugaring

desugarBDecls, desugarIPBinds :: Desugaring Binds
desugarBDecls = noDesugaring
desugarIPBinds = noDesugaring

desugarIPBind :: Desugaring IPBind
desugarIPBind = noDesugaring

desugarMatch :: Desugaring Match
desugarMatch = noDesugaring

desugarQualConDecl :: Desugaring QualConDecl
desugarQualConDecl = noDesugaring

desugarConDecl, desugarInfixConDecl, desugarRecDecl :: Desugaring ConDecl
desugarConDecl = noDesugaring
desugarInfixConDecl = noDesugaring
desugarRecDecl = noDesugaring

desugarGadtDecl :: Desugaring GadtDecl
desugarGadtDecl = noDesugaring

desugarClsDecl, desugarClsDataFam, desugarClsTyFam, desugarClsTyDef :: Desugaring ClassDecl
desugarClsDecl = noDesugaring
desugarClsDataFam = noDesugaring
desugarClsTyFam = noDesugaring
desugarClsTyDef = noDesugaring

desugarInsDecl, desugarInsType, desugarInsData, desugarInsGData :: Desugaring InstDecl
desugarInsDecl = noDesugaring
desugarInsType = noDesugaring
desugarInsData = noDesugaring
desugarInsGData = noDesugaring

desugarBangedTy, desugarUnBangedTy, desugarUnpackedTy :: Desugaring BangType
desugarBangedTy = noDesugaring
desugarUnBangedTy = noDesugaring
desugarUnpackedTy = noDesugaring

desugarUnGuardedRhs, desugarGuardedRhss :: Desugaring Rhs
desugarUnGuardedRhs = noDesugaring
desugarGuardedRhss = noDesugaring

desugarGuardedRhs :: Desugaring GuardedRhs
desugarGuardedRhs = noDesugaring

desugarFunDep :: Desugaring FunDep
desugarFunDep = noDesugaring

desugarChar, desugarString, desugarInt, desugarFrac, desugarPrimInt, desugarPrimWord, desugarPrimFloat, desugarPrimDouble, desugarPrimChar, desugarPrimString :: Desugaring Literal
desugarChar = noDesugaring
desugarString = noDesugaring
desugarInt = noDesugaring
desugarFrac = noDesugaring
desugarPrimInt = noDesugaring
desugarPrimWord = noDesugaring
desugarPrimFloat = noDesugaring
desugarPrimDouble = noDesugaring
desugarPrimChar = noDesugaring
desugarPrimString = noDesugaring

desugarXName, desugarXDomName :: Desugaring XName
desugarXName = noDesugaring
desugarXDomName = noDesugaring

desugarXAttr :: Desugaring XAttr
desugarXAttr = noDesugaring

desugarExpBracket, desugarPatBracket, desugarTypeBracket, desugarDeclBracket :: Desugaring Bracket
desugarExpBracket = noDesugaring
desugarPatBracket = noDesugaring
desugarTypeBracket = noDesugaring
desugarDeclBracket = noDesugaring

desugarIdSplice, desugarParenSplice :: Desugaring Splice
desugarIdSplice = noDesugaring
desugarParenSplice = noDesugaring

desugarPlayRisky, desugarPlaySafe :: Desugaring Safety
desugarPlayRisky = noDesugaring
desugarPlaySafe = noDesugaring

desugarStdCall, desugarCCall, desugarCPlusPlus, desugarDotNet, desugarJvm, desugarJs :: Desugaring CallConv
desugarStdCall = noDesugaring
desugarCCall = noDesugaring
desugarCPlusPlus = noDesugaring
desugarDotNet = noDesugaring
desugarJvm = noDesugaring
desugarJs = noDesugaring

desugarLanguagePragma, desugarOptionsPragma, desugarAnnModulePragma :: Desugaring ModulePragma
desugarLanguagePragma = noDesugaring
desugarOptionsPragma = noDesugaring
desugarAnnModulePragma = noDesugaring

desugarAlwaysActive, desugarActiveFrom, desugarActiveUntil :: Desugaring Activation
desugarAlwaysActive = noDesugaring
desugarActiveFrom = noDesugaring
desugarActiveUntil = noDesugaring

desugarRule :: Desugaring Rule
desugarRule = noDesugaring

desugarRuleVar, desugarTypedRuleVar :: Desugaring RuleVar
desugarRuleVar = noDesugaring
desugarTypedRuleVar = noDesugaring

desugarDeprText, desugarWarnText :: Desugaring WarningText
desugarDeprText = noDesugaring
desugarWarnText = noDesugaring

desugarPXAttr :: Desugaring PXAttr
desugarPXAttr = noDesugaring

desugarRPStar, desugarRPStarG, desugarRPPlus, desugarRPPlusG, desugarRPOpt, desugarRPOptG :: Desugaring RPatOp
desugarRPStar = noDesugaring
desugarRPStarG = noDesugaring
desugarRPPlus = noDesugaring
desugarRPPlusG = noDesugaring
desugarRPOpt = noDesugaring
desugarRPOptG = noDesugaring

desugarRPOp, desugarRPEither, desugarRPSeq, desugarRPGuard, desugarRPCAs, desugarRPAs, desugarRPParen, desugarRPPat :: Desugaring RPat
desugarRPOp = noDesugaring
desugarRPEither = noDesugaring
desugarRPSeq = noDesugaring
desugarRPGuard = noDesugaring
desugarRPCAs = noDesugaring
desugarRPAs = noDesugaring
desugarRPParen = noDesugaring
desugarRPPat = noDesugaring

desugarGenerator, desugarQualifier, desugarLetStmt, desugarRecStmt :: Desugaring Stmt
desugarGenerator = noDesugaring
desugarQualifier = noDesugaring
desugarLetStmt = noDesugaring
desugarRecStmt = noDesugaring

desugarQualStmt, desugarThenTrans, desugarThenBy, desugarGroupBy, desugarGroupUsing, desugarGroupByUsing :: Desugaring QualStmt
desugarQualStmt = noDesugaring
desugarThenTrans = noDesugaring
desugarThenBy = noDesugaring
desugarGroupBy = noDesugaring
desugarGroupUsing = noDesugaring
desugarGroupByUsing = noDesugaring

desugarFieldUpdate, desugarFieldPun, desugarFieldWildcard :: Desugaring FieldUpdate
desugarFieldUpdate = noDesugaring
desugarFieldPun = noDesugaring
desugarFieldWildcard = noDesugaring

desugarAlt :: Desugaring Alt
desugarAlt = noDesugaring

desugarUnGuardedAlt, desugarGuardedAlts :: Desugaring GuardedAlts
desugarUnGuardedAlt = noDesugaring
desugarGuardedAlts = noDesugaring

desugarGuardedAlt :: Desugaring GuardedAlt
desugarGuardedAlt = noDesugaring

