module Util.Function where

import           Startlude

infixr 9 .*
(.*) :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
(.*) = (.) . (.)
{-# INLINE (.*) #-}

infixr 9 .**
(.**) :: (b -> c) -> (a0 -> a1 -> a2 -> b) -> a0 -> a1 -> a2 -> c
(.**) = (.) . (.*)
{-# INLINE (.**) #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
