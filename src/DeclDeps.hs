{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Discover dependencies between declarations.
We say that Declaration X depends on Y if Y is used anywhere in the declaration of X.
-}
module DeclDeps (dumpTopLevelDefinitions) where

import Data.Foldable (traverse_)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import DynFlags (DynFlags)
import HieBin (hie_file_result, readHieFile)
import HieTypes (HieAST (..), HieASTs (getAsts), HieFile (..), NodeInfo (..), TypeIndex)
import HieUtils (flattenAst)
import Module (moduleName, moduleNameString, moduleUnitId, unitIdString)
import Name (nameModule_maybe)
import NameCache (NameCache, initNameCache)
import Outputable (ppr, showSDoc)
import UniqSupply (mkSplitUniqSupply)


dumpTopLevelDefinitions :: DynFlags -> FilePath -> IO ()
dumpTopLevelDefinitions dynFlags hieFilePath = withHieFile hieFilePath $ \HieFile {{-hie_module, - current module-} hie_asts} -> do
    let topLevelDeclAsts :: [HieAST TypeIndex]
        topLevelDeclAsts = getAsts hie_asts
            & Map.elems
            & concatMap flattenAst
            & filter
                (\Node {nodeInfo} ->
                    -- Assumption: which  HieAST Node constitutes a top level definition can only be determined from nodeAnnotations
                    let anns = nodeAnnotations nodeInfo
                    in (Set.member ("AbsBinds","HsBindLR") anns && Set.member ("FunBind","HsBindLR") anns && Set.member ("Match","Match") anns)
                    || Set.member ("DataDecl","TyClDecl") anns
                    || Set.member ("DataDecl","TyClDecl") anns
                )
    topLevelDeclAsts
        & mapMaybe (\Node {nodeSpan, nodeChildren} ->
            -- Assumption: The name of the top-level definition can be extracted from the first child of the definition node
            case nodeChildren of
                [] -> Nothing
                (firstChild:_) -> case firstChild of
                    Node {nodeInfo} -> case Map.toList (nodeIdentifiers nodeInfo) of
                        [] -> Nothing
                        ((Left _modName,_identDetails):_) -> Nothing
                        ((Right name,_identDetails):_) -> fmap
                            (\mn -> -- could we just use hie_module from above instead of doing this fmap?
                                ( nodeSpan
                                , Outputable.showSDoc dynFlags (ppr name)
                                , moduleNameString (moduleName mn)
                                , unitIdString (moduleUnitId mn)
                                )
                            )
                            (nameModule_maybe name)
            )
        & traverse_ print

withHieFile :: FilePath -> (HieFile -> IO a) -> IO a
withHieFile hieFilePath act = do
    nc0 <- mkNameCache
    (hieFileResult, _nc1) <- readHieFile nc0 hieFilePath
    act (hie_file_result hieFileResult)


mkNameCache :: IO NameCache
mkNameCache = do
    uniq_supply <- mkSplitUniqSupply 'z'
    return $ initNameCache uniq_supply []

data Decl = Decl
  { _declPackage  :: PackageName,
    _declModule   :: ModuleName,
    _declFunction :: FunctionName
  }
  deriving stock (Show, Eq, Ord)

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show) via Text

newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Eq, Ord, Show) via Text

newtype FunctionName = FunctionName {unFunctionName :: Text} deriving (Eq, Ord, Show) via Text

type Edge = (Decl, Decl)
