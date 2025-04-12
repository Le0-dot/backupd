{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Text.Printf (printf)
import Toml ((.=))
import qualified Toml

type StorageMap = M.Map String Storage

type EntryMap = M.Map String Entry

data Storage = Storage
  { storagePath :: T.Text,
    storageKey :: Secret
  }
  deriving (Show, Eq, Ord)

data Entry = Entry
  { entryType :: BackupType,
    entryFrom :: T.Text,
    entryTo :: T.Text,
    entrySuccessHooks :: [T.Text],
    entryFailureHooks :: [T.Text]
  }
  deriving (Show)

data BackupType = File | Directory | DockerVolume
  deriving (Show, Read, Enum, Bounded)

newtype Secret = Secret T.Text deriving (Eq, Ord)

instance Show Secret where
  show _ = "********"

storageCodec :: Toml.TomlCodec Storage
storageCodec =
  Storage
    <$> Toml.text "path" .= storagePath
    <*> Toml.diwrap (Toml.text "key") .= storageKey

entryCodec :: Toml.TomlCodec Entry
entryCodec =
  Entry
    <$> Toml.textBy showType parseType "type" .= entryType
    <*> Toml.text "from" .= entryFrom
    <*> Toml.diwrap (Toml.text "to") .= entryTo
    <*> Toml.list (Toml.text "cmd") "hook.success" .= entrySuccessHooks
    <*> Toml.list (Toml.text "cmd") "hook.failure" .= entrySuccessHooks

showType :: BackupType -> T.Text
showType File = "file"
showType Directory = "directory"
showType DockerVolume = "volume"

parseType :: T.Text -> Either T.Text BackupType
parseType "file" = Right File
parseType "directory" = Right Directory
parseType "volume" = Right DockerVolume
parseType other = Left $ "unsupported backup type: " <> other

filesIn :: String -> FilePath -> IO [FilePath]
filesIn ext dir = do
  contents <- listDirectory dir
  let files = filter (isExtensionOf ext) contents
  let paths = (dir </>) <$> files
  return paths

decodeStorage :: (MonadIO m) => StorageMap -> FilePath -> m StorageMap
decodeStorage storageMap file = do
  storage <- Toml.decodeFile storageCodec file
  let name = takeBaseName file
  return $ M.insert name storage storageMap

decodeEntry :: (MonadIO m, MonadFail m) => S.Set String -> EntryMap -> FilePath -> m EntryMap
decodeEntry storageEntries entryMap file = do
  entry <- Toml.decodeFile entryCodec file
  let storage = T.unpack $ entryTo entry
  if storage `S.member` storageEntries
    then return $ M.insert (takeBaseName file) entry entryMap
    else fail $ printf "storage with name %s was not configured" $ show storage

main :: IO ()
main = do
  storageFiles <- filesIn "storage" "configs"
  entryFiles <- filesIn "entry" "configs"

  storageMap <- foldM decodeStorage M.empty storageFiles
  entries <- forM entryFiles $ decodeEntry (M.keysSet storageMap) M.empty

  print storageMap
  print entries
