{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dump
    ( initDynFlags
    , dumpFile
    )
where

import Data.Map.Strict qualified as Map

import Data.Foldable (for_)
import GHC.Driver.Ppr (printForUser)
import GHC.Driver.Session (DynFlags, LlvmConfig (..), defaultDynFlags)
import GHC.Iface.Ext.Types
    ( HieAST (..)
    , HieASTs (getAsts)
    , HieFile (..)
    , Identifier
    , IdentifierDetails (..)
    , NodeInfo (..)
    , SourcedNodeInfo (..)
    )
import GHC.Iface.Ext.Utils (flattenAst, recoverFullType, renderHieType)
import GHC.Paths (libdir)
import GHC.SysTools (initSysTools)
import GHC.Utils.Outputable
    ( Depth (DefaultDepth)
    , Outputable
    , PrintUnqualified (QueryQualify)
    , SDoc
    , alwaysQualifyModules
    , alwaysQualifyPackages
    , comma
    , empty
    , hang
    , hsep
    , nest
    , parens
    , ppr
    , reallyAlwaysQualifyNames
    , text
    , vcat
    , ($$)
    , (<+>)
    , (<>)
    )
import Hie (withHieFile)
import System.IO (stdout)
import Prelude hiding ((<>))


initDynFlags :: IO DynFlags
initDynFlags = do
    systemSettings <- initSysTools libdir
    pure $ defaultDynFlags systemSettings (LlvmConfig [] [])


dumpFile :: DynFlags -> FilePath -> IO ()
dumpFile dynFlags hieFilePath = withHieFile hieFilePath $ \HieFile{hie_hs_file, hie_types, hie_module, hie_asts} -> do
    putStrLn $ "--- hie file path: " ++ hieFilePath
    putStrLn "--- hie_hs_file (Initial Haskell source file path)"
    putStrLn hie_hs_file
    putStrLn "--- hie_module (The module this HIE file is for)"
    putSDoc $ ppr hie_module
    putStrLn "--- hie_asts (Type-annotated abstract syntax trees)"
    let m = getAsts hie_asts
    putStrLn $ "    Contains a map of size " ++ show (Map.size m)
    for_ (Map.toList m) $ \(fs, ast) -> do
        let astWithRecoveredTypes =
                fmap (\typeIndex -> text $ renderHieType dynFlags $ recoverFullType typeIndex hie_types) ast
        putSDoc $ "AST for " <> ppr fs
        for_ (flattenAst astWithRecoveredTypes) $ \Node{nodeSpan, sourcedNodeInfo, nodeChildren} -> do
            putSDoc $
                nest 4 $
                    vcat
                        [ hsep ["Node at", ppr nodeSpan <> comma, ppr (length nodeChildren), "children"]
                        , nest 4 $ sourcedNodeInfoSDoc sourcedNodeInfo
                        ]
  where
    putSDoc :: SDoc -> IO ()
    putSDoc = printForUser dynFlags stdout qualifyEverything DefaultDepth


sourcedNodeInfoSDoc :: Outputable a => SourcedNodeInfo a -> SDoc
sourcedNodeInfoSDoc (SourcedNodeInfo m) =
    case Map.elems m of
        -- TODO print all values within the map
        (ni : _) -> nodeInfoSDoc ni
        [] -> empty
  where
    nodeInfoSDoc (NodeInfo annots nodeType nodeIdentifiers) =
        vcat
            [ "nodeAnnotations =" <+> ppr annots
            , hang ("nodeType" <+> parens (ppr (length nodeType))) 4 $
                vcat $
                    map ppr nodeType
            , hang ("nodeIdentifiers" <+> parens (ppr $ Map.size nodeIdentifiers)) 4 $
                vcat $
                    ( \(identifier, identDetails) ->
                        identifierSDoc identifier
                            $$ identifierDetailsSDoc identDetails
                    )
                        <$> Map.toList nodeIdentifiers
            ]


identifierSDoc :: Identifier -> SDoc
identifierSDoc identifier = case identifier of
    Left modName -> "ModuleName:" <+> ppr modName
    Right name -> "Name:" <+> ppr name


identifierDetailsSDoc :: Outputable a => IdentifierDetails a -> SDoc
identifierDetailsSDoc (IdentifierDetails mayType contextInfos) =
    hang "IdentifierDetails" 4 $
        vcat
            [ "Type:" <+> maybe "N/A" ppr mayType
            , "ContextInfo:" <+> ppr contextInfos
            ]


qualifyEverything :: PrintUnqualified
qualifyEverything =
    QueryQualify
        reallyAlwaysQualifyNames
        alwaysQualifyModules
        alwaysQualifyPackages
