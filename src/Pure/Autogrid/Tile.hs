{-# language LambdaCase, TypeApplications, DataKinds, KindSignatures, FlexibleInstances #-}
module Pure.Autogrid.Tile where

import Pure.Elm.Application
import GHC.TypeLits (Nat)

data Tile (width :: Nat) (height :: Nat)

{- SLOW!
    tl | Just (SomeNat width)  <- someNatVal (fromIntegral w)
       , Just (SomeNat height) <- someNatVal (fromIntegral h)
       = tile width height

{-# INLINE tile #-}
tile :: forall width height.
        ( KnownNat width, KnownNat height, Theme (Tile width height) )
     => Proxy width
     -> Proxy height
     -> View 
     -> View
tile _ _ = Themed @(Tile width height)

-}

-- This is an enumeration of the 20 tile cases generated in the simple layout 
-- algorithm. Absurdly, this inlining makes the whole layout algorithm ~6x as 
-- fast, reducing time spent generating batches of 4 12x18 layouts from 30ms to
-- 5ms. Integer and Nat are /really/ inefficient in GHCJS. Note that this set 
-- of dimensions could simply be reduced by dividing them all by 2, but the 
-- original intention was that the base unit is in inches, so a (2,2) is 
-- 2in x 2in. Web rendering doesn't allow us to use inches in this way because 
-- browsers always map one inch to 96px, regardless of the screen DPI. 
-- Ultimately, I still use the CSS inch as it guarantees consistent rendering 
-- and remains convenient with this style of grid.
tile :: (Int,Int) -> (View -> View)
tile = \case
  (2,2)   -> Themed @(Tile 2 2)
  (2,4)   -> Themed @(Tile 2 4)
  (2,6)   -> Themed @(Tile 2 6)
  (2,8)   -> Themed @(Tile 2 8)
  (4,2)   -> Themed @(Tile 4 2)
  (4,4)   -> Themed @(Tile 4 4)
  (4,6)   -> Themed @(Tile 4 6)
  (4,8)   -> Themed @(Tile 4 8)
  (6,2)   -> Themed @(Tile 6 2)
  (6,4)   -> Themed @(Tile 6 4)
  (6,6)   -> Themed @(Tile 6 6)
  (6,8)   -> Themed @(Tile 6 8)
  (10,2)  -> Themed @(Tile 10 2)
  (10,4)  -> Themed @(Tile 10 4)
  (10,6)  -> Themed @(Tile 10 6)
  (10,8)  -> Themed @(Tile 10 8)
  (12,2)  -> Themed @(Tile 12 2)
  (12,4)  -> Themed @(Tile 12 4)
  (12,6)  -> Themed @(Tile 12 6)
  _       -> Themed @(Tile 12 8)

instance Theme (Tile 2 2)
instance Theme (Tile 2 4)
instance Theme (Tile 2 6)
instance Theme (Tile 2 8)
instance Theme (Tile 4 2)
instance Theme (Tile 4 4)
instance Theme (Tile 4 6)
instance Theme (Tile 4 8)
instance Theme (Tile 6 2)
instance Theme (Tile 6 4)
instance Theme (Tile 6 6)
instance Theme (Tile 6 8)
instance Theme (Tile 10 2)
instance Theme (Tile 10 4)
instance Theme (Tile 10 6)
instance Theme (Tile 10 8)
instance Theme (Tile 12 2)
instance Theme (Tile 12 4)
instance Theme (Tile 12 6)
instance Theme (Tile 12 8)
