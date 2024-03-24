{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dump
    ( initDynFlags
    , dumpFile
    )
where

import Data.Map.Strict qualified as Map

import Data.Foldable (for_)
import Data.Tree qualified as Tree
import GHC.Driver.Ppr (printForUser, showSDoc)
import GHC.Driver.Session (DynFlags, LlvmConfig (..), defaultDynFlags)
import GHC.Iface.Ext.Types
    ( HieAST (..)
    , HieASTs (getAsts)
    , HieFile (..)
    , Identifier
    , IdentifierDetails (..)
    , NodeInfo (..)
    , NodeOrigin (..)
    , SourcedNodeInfo (..)
    , Span
    )
import GHC.Iface.Ext.Utils (recoverFullType, renderHieType)
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
    for_ (Map.toList m) $ \(hiePath, hieAst) -> do
        let astWithRecoveredTypes =
                fmap (\typeIndex -> text $ renderHieType dynFlags $ recoverFullType typeIndex hie_types) hieAst
        putSDoc $ "AST for " <> ppr hiePath
        putStrLn $
            drawTree $
                showNode myShowSDoc <$> toTree myShowSDoc astWithRecoveredTypes
  where
    putSDoc :: SDoc -> IO ()
    putSDoc = printForUser dynFlags stdout qualifyEverything DefaultDepth
    myShowSDoc = showSDoc dynFlags


toTree :: (SDoc -> String) -> HieAST SDoc -> Tree.Tree (SourcedNodeInfo SDoc, Span)
toTree showSDoc_ Node{nodeSpan, sourcedNodeInfo, nodeChildren} =
    Tree.Node (sourcedNodeInfo, nodeSpan) (toTree showSDoc_ <$> nodeChildren)


showNode :: (SDoc -> String) -> (SourcedNodeInfo SDoc, Span) -> String
showNode showSDoc_ (sourcedNodeInfo, spn) =
    showSDoc_ $
        vcat
            [ hsep ["Node at", ppr spn <> comma]
            , nest 4 $ sourcedNodeInfoSDoc sourcedNodeInfo
            ]


-- Copied from containers' Data.Tree, modified with nicer box-drawing characters
drawTree :: Tree.Tree String -> String
drawTree = unlines . draw


draw :: Tree.Tree String -> [String]
draw (Tree.Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "│" : shift "└─ " "   " (draw t)
    drawSubTrees (t : ts) =
        "│" : shift "├─ " "│  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)


sourcedNodeInfoSDoc :: Outputable a => SourcedNodeInfo a -> SDoc
sourcedNodeInfoSDoc (SourcedNodeInfo m) =
    vcat $
        ( \(nodeOrigin, nodeInfo) ->
            case nodeOrigin of
                SourceInfo -> hang "SourceInfo" 4 $ nodeInfoSDoc nodeInfo
                GeneratedInfo -> hang "GeneratedInfo" 4 $ nodeInfoSDoc nodeInfo
        )
            <$> Map.toList m
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
