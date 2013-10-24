-- | Types for working with PDFs.
module Graphics.PDF.Types
    ( -- * Core types
      Object(..)
    , Number(..)
    , ObjectID(..)
    , Array
    , Dict
      -- * Constructors
    , array
    , dict
    ) where

import qualified Data.ByteString     as B
import           Data.Fixed
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | A PDF object.
data Object
    = Null
    | Bool !Bool
    | Number !Number
    | String !B.ByteString
    | Name !B.ByteString
    | Array !Array
    | Dict !Dict
    | Reference !ObjectID
    | Indirect !ObjectID !Object
    deriving (Eq, Show)

-- | A PDF number object.
data Number
    = I !Int
    | F !(Fixed E6)
    deriving (Eq, Show)

-- | An object identifier, consisting of an object number and a generation
-- number.
data ObjectID = ObjectID !Int !Int deriving (Eq, Ord, Show)

-- | A PDF array object.
type Array = Vector Object

-- | A PDF dictionary object.
type Dict = HashMap B.ByteString Object

-- | Construct an 'Object' from a list of values.
array :: [Object] -> Object
array = Array . V.fromList

-- | Create an 'Object' from a list of name-value pairs.
dict :: [(B.ByteString, Object)] -> Object
dict = Dict . H.fromList
