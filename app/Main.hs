{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Arrow (Arrow (arr, first, (***)), (>>>))
import Control.Monad (forM, forM_)
import Data.Composition ((.:.))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import System.Directory
import System.FilePath (isExtensionOf, (</>))
import Text.Printf (printf)
import Toml (TomlCodec, decodeFile, diwrap, list, text, textBy, (.=))

data Storage = Storage
  { storageName :: Text,
    storagePath :: Text,
    storageKey :: Secret
  }
  deriving (Show, Eq, Ord)

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

newtype Secret = Secret Text deriving (Eq, Ord)

instance Show Secret where
  show _ = "********"

storageFileCodec :: TomlCodec [Storage]
storageFileCodec = Toml.list storageCodec "storage"

storageCodec :: TomlCodec Storage
storageCodec =
  Storage
    <$> Toml.text "name" .= storageName
    <*> Toml.text "path" .= storagePath
    <*> Toml.diwrap (Toml.text "key") .= storageKey

entryCodec :: TomlCodec Entry
entryCodec =
  Entry
    <$> Toml.textBy showType parseType "type" .= entryType
    <*> Toml.text "from" .= entryFrom
    <*> Toml.diwrap (Toml.text "to") .= entryTo
    <*> Toml.list (Toml.text "cmd") "hook.success" .= entrySuccessHooks
    <*> Toml.list (Toml.text "cmd") "hook.failure" .= entrySuccessHooks

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

readStorageFile :: FilePath -> IO [(Storage, FilePath)]
readStorageFile file = do
  contents <- Toml.decodeFile storageFileCodec file
  return $ (,file) <$> contents

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs $ tail xs

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe f x
  | f x = Just x
  | otherwise = Nothing

notUniqueWith :: (Ord a) => (a -> a -> Bool) -> [a] -> [(a, a)]
notUniqueWith f xs = mapMaybe func pairs
  where
    pairs = pairwise $ sort xs
    func = filterMaybe (uncurry f)

failCollidingStorage :: Text -> FilePath -> FilePath -> IO ()
failCollidingStorage = fail .:. printf "storage entries with the same name: %s in %s and %s"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

to3 :: ((a, b), c) -> (a, b, c)
to3 ((x, y), z) = (x, y, z)

main :: IO ()
main = do
  storageFiles <- filesIn "storage" "configs"
  entryFiles <- filesIn "entry" "configs"

  storage <- concat <$> forM storageFiles readStorageFile
  entries <- forM entryFiles $ Toml.decodeFile entryCodec

  let repeating = notUniqueWith ((==) `on` (storageName . fst)) storage
  let failInputs = repeating <&> ((first storageName *** snd) >>> arr to3)

  forM_ failInputs $ uncurry3 failCollidingStorage

  print storage
  print entries
