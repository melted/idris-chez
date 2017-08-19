module Chez.Optimization(usedDecls) where

import IRTS.Lang
import IRTS.Simplified

import Idris.Core.TT

import qualified Data.Set as S
import qualified Data.Map as M

import Data.List

-- | Find the used declarations by starting from
-- a list of roots.
usedDecls :: [Name] -> [(Name, SDecl)] -> S.Set Name
usedDecls roots decls = usedDecls' roots S.empty
    where
        declMap = M.fromList decls
        findUsages done n = case M.lookup n declMap of
                                Just d -> filter (`S.notMember` done) 
                                            $ filter(`M.member` declMap)
                                                $ usedDecl d
                                Nothing -> []
        usedDecls' :: [Name] -> S.Set Name -> S.Set Name
        usedDecls' [] acc = acc     
        usedDecls' (x:xs) acc = let done = S.insert x acc in
                                     usedDecls' (findUsages done x ++ xs) done

usedDecl :: SDecl -> [Name]
usedDecl (SFun _ _ _ expr) = usedExpr expr

usedExpr :: SExp -> [Name]
usedExpr (SV (Glob n)) = [n]
usedExpr (SApp _ n _) = [n]
usedExpr (SLet _ exp body) = usedExpr exp ++ usedExpr body
usedExpr (SCase _ _ alts) = concatMap usedAlt alts
usedExpr (SChkCase _ alts) = concatMap usedAlt alts
usedExpr _ = []

usedAlt :: SAlt -> [Name]
usedAlt (SConCase _ _ _ _ exp) = usedExpr exp
usedAlt (SConstCase _ exp) = usedExpr exp
usedAlt (SDefaultCase exp) = usedExpr exp