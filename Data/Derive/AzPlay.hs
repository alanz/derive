module Data.Derive.AzPlay(makeAzPlay) where

{-
import Prelude

test :: MType

type PFMType =
      U                          -- TyInt
  :+: I MType :*: I MType :*:U   -- TyTup

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
        [-- TypeDecl sl (Ident $ "PF" ++ n1) [] typ
         TypeDecl sl (Ident $ "PF" ++ n1) [] types

        ]
    where
        typR = dataDeclType d `TyFun` dataDeclType d
        -- typF = fromBangType $ fromJust $ lookup field $ concatMap ctorDeclFields $ dataDeclCtors d

        -- n = "[" ++ (concatMap bshow binds) ++ "]"
        -- n = "[" ++ (showOut qual) ++ "]"
        -- n1 = "[" ++ (concatMap doqcd qual) ++ "]"
        (Ident n1) = n
        -- binds = []

        typ = TyVar (Ident "bar")

        doqcd (QualConDecl _ binds ctx condecl) = "(QualConDecl " ++ cdshow condecl ++ ")"

        types = case qual of
          [] -> TyVar (Ident "oops, should not happen")
          xs -> combineQCD $ map toTyCon xs


showOut x = unlines $ map prettyPrint x

cdshow (ConDecl n ts) = "(ConDecl " ++ show n ++ show ts ++ ")"
cdshow (InfixConDecl bt1 n bt2) = "(InfixConDecl " ++ show bt1 ++ show n ++ show bt2 ++ ")"
cdshow (RecDecl n d) = "(RecDecl " ++ show n ++ show d ++ ")"

bshow (KindedVar n k) = "KindedVar " ++ show n
bshow (UnkindedVar n) = "UnkindedVar " ++ show n

combineQCD [x] = x
combineQCD (x:y:xs) = combineQCD ((TyInfix x (UnQual (Symbol ":+:")) y):xs)

toTyCon :: QualConDecl -> Type
toTyCon (QualConDecl _sl _vbs _ctx (ConDecl n bts)) =
  case bts of
    [] -> (TyCon (UnQual (Ident "U")))
    _ -> combineTC $ map toMRConstructor bts

combineTC [x] = ((TyInfix x (UnQual (Symbol ":*:")) (TyCon (UnQual (Ident "U")))))
-- combineTC (x:y:xs) = combineTC ((TyInfix x (UnQual (Symbol ":*:")) y):xs)
combineTC (x:y:xs) = (TyInfix x (UnQual (Symbol ":*:")) (combineTC (y:xs)))

toMRConstructor (UnBangedTy t) = TyApp (TyCon (UnQual (Ident "I"))) t
toMRConstructor ff = (TyCon (UnQual (Ident (show ff))))

{-

Starting  from

data MType = TyInt | TyTup MType MType

-- Got

type PF[
(QualConDecl (ConDecl Ident "TyInt"[]))
(QualConDecl (ConDecl Ident "TyTup"[UnBangedTy (TyCon (UnQual (Ident "MType"))),
                                    UnBangedTy (TyCon (UnQual (Ident "MType")))]))]
     = bar

So : QualConDecl:ConDecl are combined with :+:
     Each list of constructors is combined with :*:

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
