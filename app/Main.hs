{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.Text (Text)
import System.Directory
import System.FilePath ((</>))
import Toml (TomlCodec, decodeFile, dioptional, diwrap, list, table, text, textBy, (.=))

data Config = Config
  { configBackup :: Backup,
    configBackend :: Maybe Backend,
    configSuccessHooks :: [Text],
    configFailureHooks :: [Text]
  }
  deriving (Show)

data Backup = Backup
  { configType :: BackupType,
    configFrom :: Location
  }
  deriving (Show)

data BackupType = File | Directory | DockerVolume
  deriving (Show, Read, Enum, Bounded)

newtype Location = Location Text deriving (Show)

data Backend = Backend
  { configTo :: Location,
    configKey :: Secret
  }
  deriving (Show)

newtype Secret = Secret Text

instance Show Secret where
  show _ = "********"

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table backupCodec "backup" .= configBackup
    <*> Toml.dioptional (Toml.table backendCodec "backend") .= configBackend
    <*> Toml.list (Toml.text "cmd") "hooks.success" .= configSuccessHooks
    <*> Toml.list (Toml.text "cmd") "hooks.failure" .= configSuccessHooks

backupCodec :: TomlCodec Backup
backupCodec =
  Backup
    <$> Toml.textBy showType parseType "type" .= configType
    <*> Toml.diwrap (Toml.text "from") .= configFrom

backendCodec :: TomlCodec Backend
backendCodec =
  Backend
    <$> Toml.diwrap (Toml.text "to") .= configTo
    <*> Toml.diwrap (Toml.text "key") .= configKey

showType :: BackupType -> Text
showType File = "file"
showType Directory = "directory"
showType DockerVolume = "volume"

parseType :: Text -> Either Text BackupType
parseType "file" = Right File
parseType "directory" = Right Directory
parseType "volume" = Right DockerVolume
parseType other = Left $ "Unsupported backup type: " <> other

readConfigs :: FilePath -> IO [Config]
readConfigs dir = do
  files <- listDirectory dir
  let paths = (dir </>) <$> files
  forM paths $ Toml.decodeFile configCodec

main :: IO ()
main = do
  configs <- readConfigs "configs"
  print configs
