module Hie (withHieFile) where

import Data.IORef (newIORef)
import GHC.Iface.Env (updNameCache)
import GHC.Iface.Ext.Binary (NameCacheUpdater (..), hie_file_result, readHieFile)
import GHC.Iface.Ext.Types (HieFile)
import GHC.Types.Name.Cache (NameCache, initNameCache)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)


withHieFile :: FilePath -> (HieFile -> IO a) -> IO a
withHieFile hieFilePath act = do
    ncu <- initNCU
    hieFileResult <- readHieFile ncu hieFilePath
    act (hie_file_result hieFileResult)
  where
    initNCU = do
        nameCache <- mkNameCache
        ncRef <- newIORef nameCache
        pure $ NCU (updNameCache ncRef)


mkNameCache :: IO NameCache
mkNameCache = do
    uniq_supply <- mkSplitUniqSupply 'z'
    return $ initNameCache uniq_supply []
