{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}


module Cgm.Data.Structured.Derive (
    deriveStructured
) where

import Language.Haskell.TH
import Control.Monad

-- TODO: Deriving an instance is problematic since it must be exported. And most often this is
-- undesirable because it breaks the type abstraction. Instead we should derive
-- a method which provides the bijection, and modules which perform the derivation 
-- would then be free to define an instance and/or use the bijection. We may want to
-- derive a structure type also.

--instance Structured a => Structured [a] where
--    type Structure [a] = Either () (a, [a])
--    structure [] = Left ()
--    structure (x : xs) = Right (x, xs)
--    fromStructure (Left ()) = []
--    fromStructure (Right (x, xs)) = (x : xs)

--[InstanceD [ClassP Structured.Structured [VarT a_0]]
--           (AppT (ConT Structured.Structured)
--                 (AppT ListT (VarT a_0)))
--           [TySynInstD Structure
--                       [AppT ListT (VarT a_1)]
--                       (AppT (AppT (ConT Data.Either.Either) (ConT GHC.Unit.()))
--                             (AppT (AppT (TupleT 2) (VarT a_1))
--                                   (AppT ListT (VarT a_1)))),
--            FunD structure [Clause [ConP GHC.Types.[] []]
--                                   (NormalB (AppE (ConE Data.Either.Left) (ConE GHC.Unit.()))) [],
--                            Clause [InfixP (VarP x_2) GHC.Types.: (VarP xs_3)]
--                                   (NormalB (AppE (ConE Data.Either.Right) (TupE [VarE x_2,VarE xs_3]))) []],
--            FunD fromStructure [Clause [ConP Data.Either.Left [ConP GHC.Unit.() []]]
--                                       (NormalB (ConE GHC.Types.[])) [],
--                                Clause [ConP Data.Either.Right [TupP [VarP x_4,VarP xs_5]]]
--                                       (NormalB (InfixE (Just (VarE x_4)) (ConE GHC.Types.:) (Just (VarE xs_5)))) []]]]

-- Options for defintion of generic persistence

-- http://www.cse.unsw.edu.au/~chak/papers/instant-generics.pdf is interresting but its
-- implementation instant-generics, does not yet come with a Template Haskell
-- function to derive Representable instances

-- See also "A generic deriving mechanism for Haskell, Draft"
-- (http://www.dreixel.net/research/pdf/gdmh_draft.pdf)
-- To be presented at Haskell Symposium 2010, in September
-- Implementation is in UHC, not GHC for now.

-- GDMH seems very powerful, but it appears we do not need tagging and meta-information.
-- It also avoids using type families and functional dependencies, to show that their
-- design is workable without those new features. But here we can use type famillies
-- as in instant-generics. We could use the instant-generics design (and library).
-- We might not need to use the shallow representation, but if we stay close to
-- their design we will have less thinking to do, and we may be able to use
-- their library directly (and add TH to derive instances). The shallow representation appears
-- necessary to support subuniverses for specific generic methods (something we think
-- we do no need at this point).

-- DEBUG with -ddump-splices

deriveStructured :: Name -> Q [Dec]
deriveStructured typName =
  do (TyConI d) <- reify typName
     (type_name,tvars,_,constructors) <- typeInfo (return d)
     appliedType <- appsT $ conT' type_name : map (varT . fromTyVar) tvars
     let structureType = tySynInstD (mkName "Structure") [return appliedType] $
                                     nestedEitherT $ map (nestedTupT . map (return . snd) . snd) constructors
         structureFun = do clauses <- mapM structureClause constructors
                           return $ FunD (mkName "structure") $ addETags clauses
         structureClause (conName, components) =
           do vars <- newNames "a" components
              clause [conP conName $ map varP vars] (normalB $ nestedTupE $ map varE vars) []
         fromStructureFun =
           do clauses <- mapM fromStructureClause constructors
              return $ FunD (mkName "fromStructure") $ addPTags clauses
         fromStructureClause (conName, components) =
           do vars <- newNames "s" components
              clause [nestedTupP $ map varP vars] (normalB (appsE (conE conName : map varE vars))) []
         in sequence [instanceD (cxt []) (appT (conT $ mkName "Structured") (return appliedType))
                                [structureType, structureFun, fromStructureFun]]

conT' name = if nameBase name == "[]" then listT else conT name

-- A foldr with special cases for empty lists and singletons
nested :: b -> (a -> b) -> (a -> b -> b) -> [a] -> b
nested empty single pair [] = empty
nested empty single pair (x:[]) = single x
nested empty single pair (x:xs) = pair x $ nested empty single pair xs

nestedTupE = nested (tupE []) id (\a b -> tupE [a, b])
nestedTupT = nested (tupT []) id (\a b -> tupT [a, b])
nestedTupP = nested (tupP []) id (\a b -> tupP [a, b])
nestedEitherT = nested (error "nestedEitherT []") id eitherT

addETags = nested [] return $ \c cs -> mapClauseBodyE leftETag c : map (mapClauseBodyE rightETag) cs
addPTags = nested [] return $ \c cs -> mapClausePat1 leftPTag c : map (mapClausePat1 rightPTag) cs
leftETag = AppE $ ConE $ mkName "Left"
rightETag = AppE $ ConE $ mkName "Right"
leftPTag p = ConP (mkName "Left") [p]
rightPTag p = ConP (mkName "Right") [p]
mapClauseBodyE f (Clause ps b ds) = Clause ps (mapBodyE f b) ds
mapClausePat1 f (Clause [p] b ds) = Clause [f p] b ds
mapBodyE f (GuardedB gs) = GuardedB $ map (\p -> (fst p, f $ snd p)) gs
mapBodyE f (NormalB e) = NormalB $ f e

appsT :: [TypeQ] -> TypeQ
appsT [] = error "appsT []"
appsT [x] = x
appsT (x:y:zs) = appsT $ appT x y : zs

tupT ts = appsT $ tupleT (length ts) : ts

eitherT ta tb = appsT [conT (mkName "Either"), ta, tb]

fromTyVar :: TyVarBndr -> Name
fromTyVar (PlainTV v) = v
fromTyVar (KindedTV v _) = v

newNames prefix = mapM $ const $ newName prefix

-- And some borrowed helper code taken from Syb III / replib 0.2

typeInfo :: DecQ -> Q (Name, [TyVarBndr], [(Name, Int)], [(Name, [(Maybe Name, Type)])])
typeInfo m =
     do d <- m
        case d of
           d@DataD{} ->
            return (simpleName $ name d, paramsA d, consA d, termsA d)
           d@NewtypeD{} ->
            return (simpleName $ name d, paramsA d, consA d, termsA d)
           _ -> error ("derive: not a data type declaration: " ++ show d)

     where
        consA (DataD _ _ _ cs _)    = map conA cs
        consA (NewtypeD _ _ _ c _)  = [ conA c ]

        paramsA (DataD _ _ ps _ _) = ps
        paramsA (NewtypeD _ _ ps _ _) = ps

        termsA (DataD _ _ _ cs _) = map termA cs
        termsA (NewtypeD _ _ _ c _) = [ termA c ]

        termA (NormalC c xs)        = (c, map (\x -> (Nothing, snd x)) xs)
        termA (RecC c xs)           = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
        termA (InfixC t1 c t2)      = (c, [(Nothing, snd t1), (Nothing, snd t2)])

        conA (NormalC c xs)         = (simpleName c, length xs)
        conA (RecC c xs)            = (simpleName c, length xs)
        conA (InfixC _ c _)         = (simpleName c, 2)

        name (DataD _ n _ _ _)      = n
        name (NewtypeD _ n _ _ _)   = n
        name d                      = error $ show d

simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t

