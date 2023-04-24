{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Illiterate where

import Data.Array
import Data.Int

aindexAt :: (Integral w) ⇒ Array Int w → Int → w
aindexAt arrayW idx = arrayW ! idx

a16 :: Array Int Int16
a32 :: Array Int Int32
 
a16 = listArray (0, 5) [77, 777, 210]
a32 = listArray (0, 4) [666, 66, 76]

aint16 = aindexAt a16 3
aint322 = aindexAt a32 2
