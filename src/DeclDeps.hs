{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-| Discover dependencies between declarations.
We say that Declaration X depends on Y if Y is used anywhere in the declaration of X.
-}
module DeclDeps (dumpTopLevelDefinitions) where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import DynFlags (DynFlags)
import HieBin (hie_file_result, readHieFile)
import HieTypes (ContextInfo (Use), HieAST (..), HieASTs (getAsts), HieFile (..), NodeInfo (..),
                 TypeIndex, identInfo)
import HieUtils (flattenAst)
import Module (Module, moduleName, moduleNameString, moduleUnitId, unitIdString)
import Name (Name, nameModule_maybe, nameOccName, occNameString)
import NameCache (NameCache, initNameCache)
import UniqSupply (mkSplitUniqSupply)


dumpTopLevelDefinitions :: DynFlags -> FilePath -> IO ()
dumpTopLevelDefinitions _ hieFilePath = withHieFile hieFilePath $ \HieFile {hie_module, hie_asts} -> do
    let topLevelDeclAsts :: [HieAST TypeIndex]
        topLevelDeclAsts = getAsts hie_asts
            & Map.elems
            & concatMap flattenAst
            & filter isTopLevelDecl
    topLevelDeclAsts
        & mapMaybe (\hieAst -> fmap (,getUsedSymbols hieAst) (getTopLevelDeclInfo hie_module hieAst))
        & traverse_ (\(declSym, usedSyms) -> do
            Text.putStrLn (prettySymbol declSym)
            traverse_ (\s -> Text.putStrLn $ "    " <> prettySymbol s) usedSyms
        )


isTopLevelDecl :: HieAST a -> Bool
isTopLevelDecl Node {nodeInfo} =
    -- Assumption: which  HieAST Node constitutes a top level definition can only be determined from nodeAnnotations
    let anns = nodeAnnotations nodeInfo
    in (Set.member ("AbsBinds","HsBindLR") anns && Set.member ("FunBind","HsBindLR") anns && Set.member ("Match","Match") anns)
    || Set.member ("DataDecl","TyClDecl") anns
    || Set.member ("DataDecl","TyClDecl") anns


getTopLevelDeclInfo :: Module -> HieAST a -> Maybe Symbol
getTopLevelDeclInfo currentModule Node{{-nodeSpan-} nodeChildren} =
    case nodeChildren of
        [] -> Nothing
        -- Assumption: The name of the top-level definition can be extracted from the first child of the definition node
        (firstChild:_) -> case firstChild of
            -- Assumption: it's ok to just look at the first identifier (if any)
            Node {nodeInfo} -> case Map.lookupMin (nodeIdentifiers nodeInfo) of
                Just (Right name {- Lefts contain ModuleName (e.g. in imports) -}, _) -> Just $ mkSymbol name currentModule
                _ -> Nothing


getUsedSymbols :: HieAST a -> [Symbol]
getUsedSymbols =
    sortOn _symbolName
    . nubOrd
    . mapMaybe (\Node{nodeInfo} -> case Map.lookupMin (nodeIdentifiers nodeInfo) of
        Just (Right name, identDetails)
            | Set.member Use (identInfo identDetails) ->
                fmap (mkSymbol name) (nameModule_maybe name)
        _ -> Nothing)
    . flattenAst


mkSymbol :: Name -> Module -> Symbol
mkSymbol  name modul = Symbol
    (PackageName . Text.pack . unitIdString $ moduleUnitId modul)
    (ModuleName . Text.pack . moduleNameString $ moduleName modul)
    (SymbolName . Text.pack . occNameString $ nameOccName name)

withHieFile :: FilePath -> (HieFile -> IO a) -> IO a
withHieFile hieFilePath act = do
    nc0 <- mkNameCache
    (hieFileResult, _nc1) <- readHieFile nc0 hieFilePath
    act (hie_file_result hieFileResult)


mkNameCache :: IO NameCache
mkNameCache = do
    uniq_supply <- mkSplitUniqSupply 'z'
    return $ initNameCache uniq_supply []

prettySymbol :: Symbol -> Text
prettySymbol (Symbol p m s) =
    -- TODO use some formatting lib
    unSymbolName s <> Text.replicate (25 - Text.length (unSymbolName s)) " "
    <> unModuleName m <> " ("
    <> unPackageName p <> ")"

data Symbol = Symbol
  { _symbolPackage :: PackageName,
    _symbolModule  :: ModuleName,
    _symbolName    :: SymbolName
  }
  deriving stock (Show, Eq, Ord)

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show) via Text

newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Eq, Ord, Show) via Text

newtype SymbolName = SymbolName {unSymbolName :: Text} deriving (Eq, Ord, Show) via Text

type Edge = (Symbol, Symbol)
