module Data.Derive.AzPlay(makeAzPlay) where

{-
import Prelude

test :: MType

type PFMType =
      U                        -- TyInt
  :+: I Type :*: I Type :*:U   -- TyTup

-}


import Data.Derive.Internal.Derivation
import Data.Derive.Internal.Traversal
import Data.Maybe
import Language.Haskell


makeAzPlay :: Derivation
makeAzPlay  = derivationCustom "AzPlay" $
   \(_,d) -> Right $ makeUpdateField d

-- DataDecl SrcLoc DataOrNew Context Name [TyVarBind] [QualConDecl] [Deriving]
makeUpdateField :: DataDecl -> [Decl]
makeUpdateField d@(DataDecl _ don ctx n _binds qual _ds) =
        [TypeDecl sl (Ident $ "PF" ++ n) [] typ
        ,TypeDecl sl (Ident $ "PF" ++ n) [] types

        -- , TypeSig sl [name upd] (TyParen (TyFun typF typF) `TyFun` typR)
        -- ,bind upd [pVar "f",pVar "x"] $ RecUpdate (var "x") [FieldUpdate (qname field) (App (var "f") (Paren $ App (var field) (var "x")))]

        -- ,TypeSig sl [name set] (typF `TyFun` typR)
        -- ,bind set [pVar "v",pVar "x"] $ RecUpdate (var "x") [FieldUpdate (qname field) (var "v")]
        ]
    where
        -- set = field ++ "_s"
        -- upd = field ++ "_u"
        typR = dataDeclType d `TyFun` dataDeclType d
        -- typF = fromBangType $ fromJust $ lookup field $ concatMap ctorDeclFields $ dataDeclCtors d

        -- n = "[" ++ (concatMap bshow binds) ++ "]"
        -- n = "[" ++ (showOut qual) ++ "]"
        n = "[" ++ (concatMap doqcd qual) ++ "]"
        -- binds = []

        typ = TyVar (Ident "bar")

        doqcd (QualConDecl _ binds ctx condecl) = "(QualConDecl " ++ cdshow condecl ++ ")"

        types = foldl jf (TyVar (Ident "foo")) qual

        jf :: Type -> QualConDecl -> Type
        jf = undefined

showOut x = unlines $ map prettyPrint x

cdshow (ConDecl n ts) = "(ConDecl " ++ show n ++ show ts ++ ")"
cdshow (InfixConDecl bt1 n bt2) = "(InfixConDecl " ++ show bt1 ++ show n ++ show bt2 ++ ")"
cdshow (RecDecl n d) = "(RecDecl " ++ show n ++ show d ++ ")"

bshow (KindedVar n k) = "KindedVar " ++ show n
bshow (UnkindedVar n) = "UnkindedVar " ++ show n

{-
-- Got

type PF[
(QualConDecl (ConDecl Ident "TyInt"[]))
(QualConDecl (ConDecl Ident "TyTup"[UnBangedTy (TyCon (UnQual (Ident "MType"))),
                                    UnBangedTy (TyCon (UnQual (Ident "MType")))]))]
     = bar


-- Expecting
for

type PFMType =
      U                        -- TyInt
  :+: I Type :*: I Type :*:U   -- TyTup


TypeDecl
  (SrcLoc {srcFilename = "", srcLine = 0, srcColumn = 0})
  (Ident "PFMType")
  []

  (TyInfix
    (TyCon (UnQual (Ident "U")))
    (UnQual (Symbol ":+:"))
    (TyInfix
      (TyApp (TyCon (UnQual (Ident "I")))
             (TyCon (UnQual (Ident "Type"))))
      (UnQual (Symbol ":*:"))
      (TyInfix
        (TyApp (TyCon (UnQual (Ident "I")))
               (TyCon (UnQual (Ident "Type"))))
        (UnQual (Symbol ":*:"))
        (TyCon (UnQual (Ident "U"))))
    )
  )

TypeDecl (SrcLoc {srcFilename = "", srcLine = 0, srcColumn = 0})
  (Ident "PF[]") [] (TyVar (Ident "bar"))

-}
