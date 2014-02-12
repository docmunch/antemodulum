module Antemodulum (
  module Export
) where

--------------------------------------------------------------------------------

-- These modules are exported directly. If there are conflicting names among the
-- modules, this module will not compile.
import Antemodulum.Applicative as Export
import Antemodulum.Arrow as Export
import Antemodulum.Char as Export
import Antemodulum.ClassyPrelude as Export
import Antemodulum.Concurrent as Export
import Antemodulum.DeepSeq as Export
import Antemodulum.Default as Export
import Antemodulum.FilePath as Export
import Antemodulum.Functor as Export
import Antemodulum.Maybe as Export
import Antemodulum.Monad as Export
import Antemodulum.Monad.Error as Export
import Antemodulum.Monad.IO as Export
import Antemodulum.Numeric as Export
import Antemodulum.Read as Export
import Antemodulum.Safe as Export
import Antemodulum.System as Export
import Antemodulum.Text as Export
import Antemodulum.Time as Export

-- These modules are guaranteed to conflict with 'Antemodulum'. If you need to
-- use functions from one of these modules, import the module qualified but use
-- the type (synonym) exported here.
import Antemodulum.ByteString.Char8 as Export (ByteStringC)
import Antemodulum.ByteString.Lazy as Export (ByteStringL)
import Antemodulum.List.NonEmpty as Export (NonEmpty(..))
import Antemodulum.Strict as Export (EitherS, MaybeS, Pair)
import Antemodulum.Text.Lazy as Export (TextL)
