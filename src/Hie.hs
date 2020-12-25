module Hie (withHieFile) where

import HieBin (hie_file_result, readHieFile)
import HieTypes (HieFile)
import NameCache (NameCache, initNameCache)
import UniqSupply (mkSplitUniqSupply)


withHieFile :: FilePath -> (HieFile -> IO a) -> IO a
withHieFile hieFilePath act = do
    nc0 <- mkNameCache
    (hieFileResult, _nc1) <- readHieFile nc0 hieFilePath
    act (hie_file_result hieFileResult)


mkNameCache :: IO NameCache
mkNameCache = do
    uniq_supply <- mkSplitUniqSupply 'z'
    return $ initNameCache uniq_supply []
