{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dump (
    initDynFlags,
    dumpFile,
) where

import qualified Data.Map.Strict as Map

import Data.Foldable (for_)
import DynFlags (DynFlags, LlvmConfig (..), defaultDynFlags)
import GHC.Paths (libdir)
import Hie (withHieFile)
import HieTypes (
    HieAST (..),
    HieASTs (getAsts),
    HieFile (..),
    NodeInfo (..),
 )
import HieUtils (flattenAst, recoverFullType, renderHieType)
import Outputable (
    Outputable,
    PrintUnqualified (QueryQualify),
    SDoc,
    alwaysQualifyModules,
    alwaysQualifyPackages,
    comma,
    hsep,
    interpp'SP,
    nest,
    ppr,
    printForUser,
    reallyAlwaysQualifyNames,
    text,
    vcat,
    (<+>),
    (<>),
 )
import SysTools (initSysTools)
import System.IO (stdout)
import Prelude hiding ((<>))


initDynFlags :: IO DynFlags
initDynFlags = do
    systemSettings <- initSysTools libdir
    pure $ defaultDynFlags systemSettings


#if __GLASGOW_HASKELL__ >= 810
                        (LlvmConfig [] [])
#else
                        ([], [])
#endif

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
        for_ (flattenAst astWithRecoveredTypes) $ \Node{nodeSpan, nodeInfo, nodeChildren} -> do
            putSDoc $
                nest 4 $
                    vcat
                        [ hsep ["Node at", ppr nodeSpan <> comma, ppr (length nodeChildren), "children"]
                        , nest 4 $ nodeInfoSDoc nodeInfo
                        ]
  where
    putSDoc :: SDoc -> IO ()
    putSDoc = printForUser dynFlags stdout qualifyEverything


nodeInfoSDoc :: Outputable a => NodeInfo a -> SDoc
nodeInfoSDoc (NodeInfo annots nodeType nodeIdentifiers) =
    vcat
        [ "nodeAnnotations =" <+> ppr annots
        , "nodeType        =" <+> interpp'SP nodeType
        , "nodeIdentifiers =" <+> ppr nodeIdentifiers
        ]


qualifyEverything :: PrintUnqualified
qualifyEverything =
    QueryQualify
        reallyAlwaysQualifyNames
        alwaysQualifyModules
        alwaysQualifyPackages
