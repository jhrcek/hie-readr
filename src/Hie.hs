module Hie (withHieFile) where

import GHC.Iface.Ext.Binary (hie_file_result, readHieFile)
import GHC.Iface.Ext.Types (HieFile)
import GHC.Types.Name.Cache (initNameCache)


withHieFile :: FilePath -> (HieFile -> IO a) -> IO a
withHieFile hieFilePath act = do
    nameCache <- initNameCache 'z' []
    hieFileResult <- readHieFile nameCache hieFilePath
    act (hie_file_result hieFileResult)
