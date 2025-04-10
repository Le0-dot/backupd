{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.Map (Map)
import Data.Text (Text)
import System.Directory
import System.FilePath (isExtensionOf, (</>))
import Toml (TomlCodec, decodeFile, diwrap, list, tableMap, text, textBy, (.=), _KeyText) -- from tomland

newtype StorageFile = StorageFile
  { storageFileEntries :: Map Text Storage
  }
  deriving (Show)

data Storage = Storage
  { storagePath :: Text,
    storageKey :: Secret
  }
  deriving (Show)

data Entry = Entry
  { entryType :: BackupType,
    entryFrom :: Text,
    entryTo :: Text,
    entrySuccessHooks :: [Text],
    entryFailureHooks :: [Text]
  }
  deriving (Show)

data BackupType = File | Directory | DockerVolume
  deriving (Show, Read, Enum, Bounded)

newtype Secret = Secret Text

instance Show Secret where
  -- show _ = "********"
  show (Secret t) = show t

storageFileCodec :: TomlCodec StorageFile
storageFileCodec =
  StorageFile
    <$> Toml.tableMap Toml._KeyText (const storageCodec) "storage" .= storageFileEntries

storageCodec :: TomlCodec Storage
storageCodec =
  Storage
    <$> Toml.text "path" .= storagePath
    <*> Toml.diwrap (Toml.text "key") .= storageKey

entryCodec :: TomlCodec Entry
entryCodec =
  Entry
    <$> Toml.textBy showType parseType "type" .= entryType
    <*> Toml.text "from" .= entryFrom
    <*> Toml.diwrap (Toml.text "to") .= entryTo
    <*> Toml.list (Toml.text "cmd") "hooks.success" .= entrySuccessHooks
    <*> Toml.list (Toml.text "cmd") "hooks.failure" .= entrySuccessHooks

showType :: BackupType -> Text
showType File = "file"
showType Directory = "directory"
showType DockerVolume = "volume"

parseType :: Text -> Either Text BackupType
parseType "file" = Right File
parseType "directory" = Right Directory
parseType "volume" = Right DockerVolume
parseType other = Left $ "Unsupported backup type: " <> other

filesIn :: String -> FilePath -> IO [FilePath]
filesIn ext dir = do
  contents <- listDirectory dir
  let files = filter (isExtensionOf ext) contents
  let paths = (dir </>) <$> files
  return paths

readConfigs :: String -> FilePath -> TomlCodec a -> IO [a]
readConfigs ext dir codec = do
  files <- filesIn ext dir
  forM files $ Toml.decodeFile codec

readEntries :: FilePath -> IO [Entry]
readEntries dir = readConfigs "entry" dir entryCodec

readStorageFiles :: FilePath -> IO [StorageFile]
readStorageFiles dir = readConfigs "storage" dir storageFileCodec

main :: IO ()
main = do
  storage <- readStorageFiles "configs"
  entries <- readEntries "configs"
  print storage
  print entries
