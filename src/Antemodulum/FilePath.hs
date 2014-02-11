module Antemodulum.FilePath (
  -- *
  nullFP,
  stripPrefixFP,
  -- *
  isFile,
  getModified,
  getSize,
  copyFile,
  copyFileContent,
  copyPermissions,
  removeFile,
  openFile,
  withFile,
  appendFile,
  openTextFile,
  withTextFile,
  readTextFile,
  writeTextFile,
  appendTextFile,
  isDirectory,
  canonicalizePath,
  listDirectory,
  createDirectory,
  createTree,
  removeDirectory,
  removeTree,
  getWorkingDirectory,
  -- *
  HasFilePath(..),
  IsFilePath(..),
  (</>),
  (<.>),
  -- *
  dropDirAndExt,
  replaceDir,
  replaceDirAndExt,
  rewriteBase,
  -- *
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Antemodulum.Monad.IO

import Filesystem as Export (IOMode(..))
import Filesystem.Path.CurrentOS as Export hiding (concat, decode, empty, encode, fromText, null, stripPrefix, (</>), (<.>))

import qualified Filesystem as Import
import qualified Filesystem.Path.CurrentOS as Import

--------------------------------------------------------------------------------

-- | 'Import.null'
nullFP :: FilePath -> Bool
nullFP = Import.null

-- | 'Import.stripPrefix'
stripPrefixFP :: FilePath -> FilePath -> Maybe FilePath
stripPrefixFP = Import.stripPrefix

--------------------------------------------------------------------------------

isFile :: MonadIO m => FilePath -> m Bool
isFile = liftIO1 Import.isFile

getModified :: MonadIO m => FilePath -> m UTCTime
getModified = liftIO1 Import.getModified

getSize :: MonadIO m => FilePath -> m Integer
getSize = liftIO1 Import.getSize

copyFile :: MonadIO m => FilePath -> FilePath -> m ()
copyFile = liftIO2 Import.copyFile

copyFileContent :: MonadIO m => FilePath -> FilePath -> m ()
copyFileContent = liftIO2 Import.copyFileContent

copyPermissions :: MonadIO m => FilePath -> FilePath -> m ()
copyPermissions = liftIO2 Import.copyPermissions

removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO1 Import.removeFile

openFile :: MonadIO m => FilePath -> IOMode -> m Handle
openFile = liftIO2 Import.openFile

withFile :: MonadIO m => FilePath -> IOMode -> (Handle -> IO a) -> m a
withFile = liftIO3 Import.withFile

appendFile :: MonadIO m => FilePath -> ByteString -> m ()
appendFile = liftIO2 Import.appendFile

openTextFile :: MonadIO m => FilePath -> IOMode -> m Handle
openTextFile = liftIO2 Import.openTextFile

withTextFile :: MonadIO m => FilePath -> IOMode -> (Handle -> IO a) -> m a
withTextFile = liftIO3 Import.withTextFile

readTextFile :: MonadIO m => FilePath -> m Text
readTextFile = liftIO1 Import.readTextFile

writeTextFile :: MonadIO m => FilePath -> Text -> m ()
writeTextFile = liftIO2 Import.writeTextFile

appendTextFile :: MonadIO m => FilePath -> Text -> m ()
appendTextFile = liftIO2 Import.appendTextFile

isDirectory :: MonadIO m => FilePath -> m Bool
isDirectory = liftIO1 Import.isDirectory

canonicalizePath :: MonadIO m => FilePath -> m FilePath
canonicalizePath = liftIO1 Import.canonicalizePath

listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory = liftIO1 Import.listDirectory

createDirectory :: MonadIO m => Bool -> FilePath -> m ()
createDirectory = liftIO2 Import.createDirectory

createTree :: MonadIO m => FilePath -> m ()
createTree = liftIO1 Import.createTree

removeDirectory :: MonadIO m => FilePath -> m ()
removeDirectory = liftIO1 Import.removeDirectory

removeTree :: MonadIO m => FilePath -> m ()
removeTree = liftIO1 Import.removeTree

getWorkingDirectory :: MonadIO m => m FilePath
getWorkingDirectory = liftIO Import.getWorkingDirectory

--------------------------------------------------------------------------------

class HasFilePath a where
  toFilePath :: a -> FilePath

instance HasFilePath FilePath where
  toFilePath = id
instance HasFilePath String where
  toFilePath = decodeString
instance HasFilePath Text where
  toFilePath = Import.fromText

--------------------------------------------------------------------------------

class IsFilePath a where
  fromFilePath :: FilePath -> a

instance IsFilePath FilePath where
  fromFilePath = id
instance IsFilePath String where
  fromFilePath = encodeString
instance IsFilePath Text where
  fromFilePath = either id id . toText

--------------------------------------------------------------------------------

-- | Version of 'Filesystem.Path.</>' using 'HasFilePath'.
(</>) :: (HasFilePath fp1, HasFilePath fp2) => fp1 -> fp2 -> FilePath
x </> y = toFilePath x Import.</> toFilePath y

-- | Version of 'Filesystem.Path.<.>' using 'HasFilePath'.
(<.>) :: HasFilePath fp => fp -> Text -> FilePath
x <.> y = toFilePath x Import.<.> y

--------------------------------------------------------------------------------

-- | Drop the directory and one extension.
dropDirAndExt :: FilePath -> FilePath
dropDirAndExt = dropExtension . filename

-- | Replace the directory.
replaceDir :: FilePath -> FilePath -> FilePath
replaceDir f dir = dir </> filename f

-- | Replace the directory and file extension.
replaceDirAndExt :: FilePath -> FilePath -> Text -> FilePath
replaceDirAndExt f dir = replaceExtension (replaceDir f dir)

-- | Rewrite the basename (i.e. remove the extensions and directory, apply the
-- function, and recombine the components)
rewriteBase :: (Text -> Text) -> FilePath -> FilePath
rewriteBase rew f = case splitExtensions (filename f) of
  (base, exts) -> directory f </> addExtensions (rewrite base) exts
  where
    rewrite :: FilePath -> FilePath
    rewrite = toFilePath . rew . fromFilePath

