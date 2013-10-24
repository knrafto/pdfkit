-- | Parsing of PDF objects and structure.
module Graphics.PDF.Parse
    ( -- * Objects
      object
    , streamDict
      -- * File structure
    , version
    , xrefOffset
    ) where

import Graphics.PDF.Parse.File
import Graphics.PDF.Parse.Object
