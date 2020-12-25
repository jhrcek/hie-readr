{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cli (main) where

import DeclDeps (dumpDeclDeps)
import Dump (dumpFile, initDynFlags)
import Options.Applicative (
    Parser,
    command,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    showDefault,
    strArgument,
    strOption,
    subparser,
    switch,
    value,
 )
import System.Exit (die)
import System.FilePath (takeExtension)


main :: IO ()
main = do
    cmd <- parseCommand
    case cmd of
        Dump DumpOpts{hieFile}
            | takeExtension hieFile == ".hie" -> do
                dynFlags <- initDynFlags
                dumpFile dynFlags hieFile
            | otherwise -> die "The file you supplied didn't have .hie extension"
        DeclDeps DeclDepsOpts{hieDir, outFile, stripHashes} ->
            dumpDeclDeps hieDir outFile stripHashes


data Command
    = Dump DumpOpts
    | DeclDeps DeclDepsOpts
    deriving stock (Show)


newtype DumpOpts = DumpOpts
    { hieFile :: FilePath
    }
    deriving stock (Show)


data DeclDepsOpts = DeclDepsOpts
    { hieDir :: FilePath
    , outFile :: FilePath
    , stripHashes :: Bool
    }
    deriving stock (Show)


parseCommand :: IO Command
parseCommand =
    execParser $
        info
            (helper <*> commandParser)
            (fullDesc <> progDesc "Dump contents of .hie files")


commandParser :: Parser Command
commandParser =
    subparser
        ( command "dump" (info (helper <*> dumpOptions) (progDesc "Dump .hie file"))
            <> command "decl-deps" (info (helper <*> declDepsOptions) (progDesc "Dump declaration dependencies"))
        )
  where
    dumpOptions =
        Dump . DumpOpts
            <$> strArgument (metavar "HIEFILE" <> help ".hie file to dump")
    declDepsOptions =
        fmap DeclDeps $
            DeclDepsOpts
                <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault <> help "Directory containing .hie files to process")
                <*> strOption (long "out" <> metavar "FILE" <> value "all.usages" <> showDefault <> help "Output file")
                <*> switch (long "strip-hash" <> help "Strip hashes from the end of UnitIDs")
