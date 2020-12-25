{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dump (
    initDynFlags,
    dumpFile,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Foldable (for_)
import DynFlags (DynFlags, LlvmConfig (..), defaultDynFlags)
import GHC.Paths (libdir)
import Hie (withHieFile)
import HieTypes (
    HieAST (..),
    HieASTs (getAsts),
    HieFile (..),
    IdentifierDetails (..),
    NodeInfo (..),
 )
import HieUtils (flattenAst)
import Module (moduleName, moduleNameString, moduleUnitId, unitIdString)
import Name (nameModule_maybe)
import Outputable (Outputable, ppr, showSDoc)
import SysTools (initSysTools)


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
dumpFile dynFlags hieFilePath = withHieFile hieFilePath $ \HieFile{hie_hs_file, hie_module, hie_asts} -> do
    putStrLn $ "-- hie file path: " <> hieFilePath
    putStrLn "--- hie_hs_file (Initial Haskell source file path)"
    putStrLn hie_hs_file
    putStrLn "--- hie_module (The module this HIE file is for)"
    printO hie_module
    putStrLn "--- hie_asts (Type-annotated abstract syntax trees)"
    let m = getAsts hie_asts
    putStrLn $ "    Contains a map of size " <> show (Map.size m)
    for_ (Map.toList m) $ \(fs, ast) -> do
        putStrLn $ "AST for " <> showO fs
        for_ (flattenAst ast) $ \Node{nodeSpan, nodeInfo, nodeChildren} -> do
            putStrLn $ "    At " <> showO nodeSpan <> ", " <> show (length nodeChildren) <> " children"
            printNodeInfo dynFlags nodeInfo
  where
    showO :: Outputable a => a -> String
    showO = Outputable.showSDoc dynFlags . ppr
    printO = putStrLn . showO


printNodeInfo :: DynFlags -> NodeInfo a -> IO ()
printNodeInfo df (NodeInfo annots _ntype nodeIdentifiers) = do
    putStr "        Annotations: "
    print $ Set.toList annots
    putStrLn $ "        Identifiers (" <> show (Map.size nodeIdentifiers) <> ")"
    for_ (Map.toList nodeIdentifiers) $ \(ident, IdentifierDetails _idType identInfoSet) -> do
        putStrLn $ case ident of
            Left modName -> "              Module: " <> moduleNameString modName
            Right name ->
                let modul = case nameModule_maybe name of
                        Nothing -> "this module"
                        Just m ->
                            let unitIdStr = unitIdString (moduleUnitId m)
                                modNameStr = moduleNameString (moduleName m)
                             in unitIdStr <> ":" <> modNameStr
                 in "              Name: " <> Outputable.showSDoc df (ppr name) <> " (" <> modul <> ")"
        for_ identInfoSet $ \contextInfo -> putStrLn $ "                " <> show contextInfo
