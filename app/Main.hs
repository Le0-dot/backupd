{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import System.Process (CmdSpec (RawCommand), CreateProcess (..), StdStream (..), readCreateProcessWithExitCode, waitForProcess, withCreateProcess)
import Text.Printf (printf)
import Toml ((.=))
import qualified Toml

type StorageMap = M.Map String Storage

type StorageMapKeys = S.Set String

type EntryMap = M.Map String Entry

data Storage = Storage
  { storagePath :: T.Text,
    storageKey :: T.Text,
    storageKeyCommand :: T.Text
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

storageCodec :: Toml.TomlCodec Storage
storageCodec = Toml.validate (\v -> pure v) $
  Storage
    <$> Toml.text "path" .= storagePath
    <*> Toml.text "key" .= storageKey
    <*> Toml.text "key-command" .= storageKeyCommand

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

decodeEntry :: (MonadIO m, MonadFail m) => StorageMapKeys -> EntryMap -> FilePath -> m EntryMap
decodeEntry storageEntries entryMap file = do
  entry <- Toml.decodeFile entryCodec file
  let storage = T.unpack $ entryTo entry
  if storage `S.member` storageEntries
    then return $ M.insert (takeBaseName file) entry entryMap
    else fail $ printf "storage with name %s was not configured" $ show storage

data ResticCommand = Init | Check | Backup String deriving (Show)

resticCommandToArg :: ResticCommand -> [String]
resticCommandToArg Init = ["init"]
resticCommandToArg Check = ["check"]
resticCommandToArg (Backup path) = ["backup", path]

storageToResticEnv :: Storage -> [(String, String)]
storageToResticEnv storage =
  [ ("RESTIC_REPOSITORY", T.unpack $ storagePath storage),
    ("RESTIC_PASSWORD", T.unpack $ secretText $ storageKey storage)
  ]

createRestic :: ResticCommand -> Storage -> CreateProcess
createRestic command storage =
  CreateProcess
    { cmdspec = RawCommand "/usr/bin/restic" $ resticCommandToArg command,
      cwd = Nothing,
      env = Just $ storageToResticEnv storage,
      std_in = NoStream,
      std_out = Inherit,
      std_err = Inherit,
      close_fds = True,
      create_group = False,
      delegate_ctlc = False,
      detach_console = False,
      create_new_console = False,
      new_session = False,
      child_group = Nothing,
      child_user = Nothing,
      use_process_jobs = False
    }

readProcess :: CreateProcess -> IO (ExitCode, String, String)
readProcess = flip readCreateProcessWithExitCode []

processExitCode :: CreateProcess -> IO ExitCode
processExitCode = flip withCreateProcess (\_ _ _ handle -> waitForProcess handle)

initRepo :: Storage -> IO ()
initRepo storage = do
  code <- processExitCode $ createRestic Check storage
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      ec <- processExitCode $ createRestic Init storage
      case ec of
        ExitSuccess -> return ()
        ExitFailure _ -> fail ""

main :: IO ()
main = do
  storageFiles <- filesIn "storage" "example-config"
  entryFiles <- filesIn "entry" "example-config"

  storageMap <- foldM decodeStorage M.empty storageFiles
  entries <- forM entryFiles $ decodeEntry (M.keysSet storageMap) M.empty

  print storageMap
  print entries

  initRepo $ storageMap M.! "local"

  -- let proc = createRestic Check $ storageMap M.! "local"
  -- (exitCode, stdout, stderr) <- readProcess proc
  --
  -- print exitCode
  -- putStrLn stdout
  -- putStrLn stderr
