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
import Antemodulum.Default as Export
import Antemodulum.FilePath as Export
import Antemodulum.Functor as Export
import Antemodulum.Maybe as Export
import Antemodulum.Monad as Export
import Antemodulum.Monad.Error as Export
import Antemodulum.Monad.IO as Export
import Antemodulum.Numeric as Export
import Antemodulum.System as Export
import Antemodulum.Text as Export
import Antemodulum.Time as Export

-- These modules are guaranteed to conflict with 'Antemodulum'. If you need to
-- use functions from one of these modules, import the module qualified but use
-- the type (synonym) exported here.
import Antemodulum.ByteString.Char8 as Export (ByteStringC)
import Antemodulum.ByteString.Lazy as Export (ByteStringL)
import Antemodulum.List.NonEmpty as Export (NonEmpty(..))
import Antemodulum.Text.Lazy as Export (TextL)
-- These types are exported by 'Antemodulum.ClassyPrelude'.
--import Antemodulum.ByteString.Strict as Export (ByteString)
--import Antemodulum.Text.Strict as Export (Text)

-- These modules conflict with each other. If you need to import one of them in
-- a module, you will be fine. If you need both, import at least one module
-- qualified.
--
-- When modifying one of the following modules, uncomment it here to verify that
-- it does not conflict with 'Antemodulum'.
--import Antemodulum.Monad.Lazy as Export
--import Antemodulum.Monad.Strict as Export
