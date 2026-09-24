-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Temporal stream transformations.
module Copilot.Language.Operators.Temporal
  ( (++)
  , drop
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Prelude
import Copilot.Language.Stream
import Prelude ()
import qualified Prelude as P

infixr 1 ++

-- | Prepend a fixed number of samples to a stream.
--
-- The elements to be appended at the beginning of the stream must be limited,
-- that is, the list must have finite length.
--
-- Prepending elements to a stream may increase the memory requirements of the
-- generated programs (which now must hold the same number of elements in
-- memory for future processing).
(++) :: Typed a => [a] -> Stream a -> Stream a
(++) = (`Append` Nothing)

-- | Drop a number of samples from a stream.
--
-- The elements must be realizable at the present time to be able to drop
-- elements. For most kinds of streams, you cannot drop elements without
-- prepending an equal or greater number of elements to them first, as it
-- could result in undefined samples.
drop :: Typed a => Int -> Stream a -> Stream a
drop 0 s             = s
drop _ ( Const j )   = Const j
drop i ( Drop  j s ) = drop (i + j) s
drop i ( Append xs _ s )
  | i P.>= n         = drop (i - n) s
  where
    n = length xs
drop i s             = Drop (fromIntegral i)     s
