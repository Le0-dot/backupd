{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Toml (TomlCodec, decodeFileEither, diwrap, prettyTomlDecodeErrors, table, text, (.=))

data BackupConfig = BackupConfig
  { backupConfigFrom :: !From,
    backupConfigTo :: !To
  }
  deriving (Show)

data From = From
  { fromLocation :: !Location,
    fromType :: !BackUpType
  }
  deriving (Show)

data BackUpType = File | Directory | DockerVolume
  deriving (Show)

data To = To
  { toBackend :: !Backend,
    toLocation :: !Location
  }
  deriving (Show)

data Backend = Restic
  deriving (Show)

newtype Location = Location Text deriving (Show)

backupConfigCodec :: TomlCodec BackupConfig
backupConfigCodec =
  BackupConfig
    <$> Toml.table fromCodec "from" .= backupConfigFrom
    <*> Toml.table toCodec "to" .= backupConfigTo

fromCodec :: TomlCodec From
fromCodec =
  From
    <$> Toml.diwrap (Toml.text "location") .= fromLocation
    <*> pure Directory

toCodec :: TomlCodec To
toCodec =
  To
    <$> pure Restic
    <*> Toml.diwrap (Toml.text "location") .= toLocation

main :: IO ()
main = do
  tomlRes <- Toml.decodeFileEither backupConfigCodec "test.toml"
  case tomlRes of
    Left errs -> print $ Toml.prettyTomlDecodeErrors errs
    Right res -> print res
