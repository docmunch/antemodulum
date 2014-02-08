module Antemodulum.FilePath (
  concatFP,
  emptyFP,
  nullFP,
  stripPrefixFP,
  HasFilePath(..),
  IsFilePath(..),
  (</>),
  (<.>),
  dropDirAndExt,
  replaceDir,
  replaceDirAndExt,
  rewriteBase,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Antemodulum.Text.Strict (TextS)

import Filesystem.Path.CurrentOS as Export hiding (concat, empty, null, stripPrefix, (</>), (<.>))
import qualified Filesystem.Path as Import

--------------------------------------------------------------------------------

-- | 'Import.concat'
concatFP :: [FilePath] -> FilePath
concatFP = Import.concat

-- | 'Import.empty'
emptyFP :: FilePath
emptyFP = Import.empty

-- | 'Import.null'
nullFP :: FilePath -> Bool
nullFP = Import.null

-- | 'Import.stripPrefix'
stripPrefixFP :: FilePath -> FilePath -> Maybe FilePath
stripPrefixFP = Import.stripPrefix

--------------------------------------------------------------------------------

class HasFilePath a where
  toFilePath :: a -> FilePath

instance HasFilePath FilePath where
  toFilePath = id
instance HasFilePath String where
  toFilePath = fromString
instance HasFilePath TextS where
  toFilePath = fromText

--------------------------------------------------------------------------------

class IsFilePath a where
  fromFilePath :: FilePath -> a

instance IsFilePath FilePath where
  fromFilePath = id
instance IsFilePath String where
  fromFilePath = encodeString
instance IsFilePath TextS where
  fromFilePath = either id id . toText

--------------------------------------------------------------------------------

-- | Version of 'Filesystem.Path.</>' using 'HasFilePath'.
(</>) :: (HasFilePath fp1, HasFilePath fp2) => fp1 -> fp2 -> FilePath
x </> y = toFilePath x Import.</> toFilePath y

-- | Version of 'Filesystem.Path.<.>' using 'HasFilePath'.
(<.>) :: HasFilePath fp => fp -> TextS -> FilePath
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
    rewrite = fromText . rew . fromFilePath

