module Split (Split, chunk) where

import Numeric.Natural
import Data.ByteString as B

class Split a where
  splitAt :: Natural -> a -> (a, a)

instance Split ByteString where
  splitAt = B.splitAt . fromIntegral . toInteger

instance Split [a] where
  splitAt = Prelude.splitAt . fromIntegral . toInteger


chunk :: (Split a, Monoid a, Eq a) => Natural -> a -> [a]
chunk n m
    | m == mempty = mempty
    | otherwise = 
        let ni = fromInteger $ toInteger n in
          let (chunk', rest) = Split.splitAt ni m in
            chunk' : chunk n rest
