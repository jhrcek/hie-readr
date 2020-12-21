{-# LANGUAGE DerivingVia #-}
{-| Discover dependencies between declarations.
We say that Declaration X depends on Y if Y is used anywhere in the declaration of X.
-}
module DeclDeps where

import Data.Text (Text)

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
