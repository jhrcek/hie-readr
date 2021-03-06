{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Discover dependencies between declarations.
We say that Declaration X depends on Y if Y is used anywhere in the declaration of X.
-}
module DeclDeps (dumpDeclDeps) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isLower, isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Hie (withHieFile)
import HieTypes (
    ContextInfo (Use),
    HieAST (..),
    HieASTs (getAsts),
    HieFile (..),
    NodeInfo (..),
    TypeIndex,
    identInfo,
 )
import HieUtils (flattenAst)
import Module (Module, moduleName, moduleNameString, moduleUnitId, unitIdString)
import Name (Name, nameModule_maybe, nameOccName, occNameString)
import System.Directory (
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    withCurrentDirectory,
 )
import System.Exit (die)
import System.FilePath (isExtensionOf)


extractDeclDeps :: FilePath -> IO DeclDeps
extractDeclDeps hieFilePath = withHieFile hieFilePath $ \HieFile{hie_module, hie_asts} -> do
    let topLevelDeclAsts :: [HieAST TypeIndex]
        topLevelDeclAsts =
            getAsts hie_asts
                & Map.elems
                & concatMap findTopLevelDeclAsts

    topLevelDeclAsts
        & mapMaybe (\hieAst -> fmap (,getUsedSymbols hieAst) (getTopLevelDeclSymbol hie_module hieAst))
        & pure


findTopLevelDeclAsts :: HieAST a -> [HieAST a]
findTopLevelDeclAsts ast
    | isTopLevelDecl ast = [ast]
    | otherwise = concatMap findTopLevelDeclAsts (nodeChildren ast)


isTopLevelDecl :: HieAST a -> Bool
isTopLevelDecl Node{nodeInfo} =
    -- Assumption: which  HieAST Node constitutes a top level definition can only be determined from nodeAnnotations
    let anns = nodeAnnotations nodeInfo
     in (Set.member ("AbsBinds", "HsBindLR") anns && Set.member ("FunBind", "HsBindLR") anns && Set.member ("Match", "Match") anns)
            || Set.member ("DataDecl", "TyClDecl") anns
            || Set.member ("DataDecl", "TyClDecl") anns


getTopLevelDeclSymbol :: Module -> HieAST a -> Maybe Symbol
getTopLevelDeclSymbol currentModule Node{nodeChildren} =
    case nodeChildren of
        [] -> Nothing
        -- Assumption: The name of the top-level definition can be extracted from the first child of the definition node
        (firstChild : _) -> case firstChild of
            -- Assumption: it's ok to just look at the first identifier (if any)
            Node{nodeInfo} -> case Map.lookupMin (nodeIdentifiers nodeInfo) of
                Just (Right name {- Lefts contain ModuleName (e.g. in imports) -}, _) -> Just $ mkSymbol name currentModule
                _ -> Nothing


getUsedSymbols :: HieAST a -> [Symbol]
getUsedSymbols =
    sortOn symbolName
        . nubOrd
        . mapMaybe
            ( \Node{nodeInfo} -> case Map.lookupMin (nodeIdentifiers nodeInfo) of
                Just (Right name, identDetails)
                    | Set.member Use (identInfo identDetails) ->
                        fmap (mkSymbol name) (nameModule_maybe name)
                _ -> Nothing
            )
        . flattenAst


mkSymbol :: Name -> Module -> Symbol
mkSymbol name modul =
    Symbol
        (PackageName . Text.pack . unitIdString $ moduleUnitId modul)
        (ModuleName . Text.pack . moduleNameString $ moduleName modul)
        (SymbolName . Text.pack . occNameString $ nameOccName name)


data Symbol = Symbol
    { symbolPackage :: PackageName
    , symbolModule :: ModuleName
    , symbolName :: SymbolName
    }
    deriving stock (Show, Eq, Ord)


newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show) via Text


newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Eq, Ord, Show) via Text


newtype SymbolName = SymbolName {unSymbolName :: Text} deriving (Eq, Ord, Show) via Text


{- | Declaration dependencies:
 for each top level definition we have a list of all symbols used within its definition
-}
type DeclDeps = [(Symbol, [Symbol])]


-- UnitId often contain some kind of disambiguating hash, which we don't care about
-- "optparse-applicative-0.15.1.0-8iKVDKS5G4m7jqr9SztVW9" -> "optparse-applicative-0.15.1.0"
-- NOTE: some hashes don't have any digits, e.g. "quad-0.0.0-JIKQGKGJTRzJUDZPluENan"
stripHash :: PackageName -> PackageName
stripHash (PackageName n) =
    PackageName $
        if Text.any isLower suffix && Text.any isUpper suffix && Text.length suffix >= 20
            then Text.dropEnd (Text.length suffix + 1) n
            else n
  where
    suffix = Text.takeWhileEnd (/= '-') n


-- | Recursively search for @.hie@ and @.hie-boot@  files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
    isFile <- doesFileExist path
    if isFile && ("hie" `isExtensionOf` path || "hie-boot" `isExtensionOf` path)
        then do
            path' <- canonicalizePath path
            return [path']
        else do
            isDir <- doesDirectoryExist path
            if isDir
                then do
                    cnts <- listDirectory path
                    withCurrentDirectory path $ foldMap getHieFilesIn cnts
                else return []


-- | Find all @.hie@ files in given directory and extract DeclDeps from them
getDeclDepsInDir :: FilePath -> IO DeclDeps
getDeclDepsInDir dirWithHieFiles = do
    hieFiles <- getHieFilesIn dirWithHieFiles
    when (null hieFiles) $ die $ "No .hie files found in " <> dirWithHieFiles
    concat <$> traverse extractDeclDeps hieFiles


dumpDeclDeps :: FilePath -> FilePath -> Bool -> IO ()
dumpDeclDeps dirWithHieFiles targetFile stripHashes = do
    deps <- getDeclDepsInDir dirWithHieFiles
    -- TODO proper serialization
    writeFile targetFile $ unlines $ fmap (show . bimap toTriple (fmap toTriple)) deps
  where
    toTriple (Symbol p m n) =
        ( unPackageName $ if stripHashes then stripHash p else p
        , unModuleName m
        , unSymbolName n
        )
