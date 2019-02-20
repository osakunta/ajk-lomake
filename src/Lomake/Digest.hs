module Lomake.Digest where

import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as LBS

newtype Digest = Digest BS.ByteString
  deriving (Eq)
