module Antemodulum.Safe (
  module Export
) where

--------------------------------------------------------------------------------

import Safe as Export
  (headDef,                    headNote,
                  initMay,     initNote,
   lastDef,                    lastNote,
   minimumDef,                 minimumNote,
   maximumDef,                 maximumNote,
   readDef,                    readNote,
                  tailMay,     tailNote,
   fromJustDef,                fromJustNote,
   lookupJustDef,              lookupJustNote)
