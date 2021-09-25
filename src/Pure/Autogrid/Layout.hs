{-# language DuplicateRecordFields, PartialTypeSignatures, PatternSynonyms, TypeApplications, BangPatterns, RankNTypes, ScopedTypeVariables, BlockArguments, KindSignatures, ViewPatterns, DataKinds, RecordWildCards, TypeFamilies, OverloadedStrings, PostfixOperators #-}
module Pure.Autogrid.Layout (layout,Layout(..),defaultWeights,feature,Grid,Container) where

import Pure.Autogrid.Tile

import Pure.Elm as Pure hiding (children,select,layout)
import Pure.Random ( Generator(generate), Seed )
import Pure.Random.Distributions ( categorical )
import Pure.Maybe ()
import Pure.Intersection ( pattern RootMargin )
import qualified Pure.Stream as Stream

import qualified Data.Vector as V

import qualified Data.List as List

import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, Nat, natVal )

import Prelude hiding (max,min)

{-
This needs work to allow type-level configuration of the grid size as well as 
the container size, but I'm not sure what predicates guarantee safe (gapless)
generation of some NxM grid - other than including a 1x1 tile (which produces 
unappealing layouts). This module makes a few assumptions to make the resulting 
layout algorithm linear:

1) Layout starts top-left and works left-to-right and top-to-bottom filling space
   while attempting to limit the number of 2-by tiles.
2) The content doesn't dictate the tile size - the tile size dictates the way 
   the content is rendered to reduce constraints on tile production.

-}

layout :: Layout st a -> View
layout l =
  let
    producer = articleStream l
    consumer = Stream.toList
    features = Themed @Container def
    children :: Stream.Step => _
    children = [ Stream.stepper <| RootMargin (400px) ]
  in
    Stream.stream Stream.Streamer {..}

defaultWeights :: V.Vector Double
defaultWeights = V.fromList [0.1,0.8,1.2,1.5,1.7]
-- defaultWeights = V.fromList [0.2,0.5,1.3,1.4,1.8]
      -- (fmap ((+ 0.1) . log) [1..5])

data Layout st a = Layout
  { masthead :: View
  , weights  :: V.Vector Double
  , seed     :: Seed
  , chunk    :: Int
  , page     :: Int
  , request  :: Int -> IO [a]
  , initial  :: st
  , tile     :: (Int,Int) -> (View -> View)
  , render   :: st -> (Int,Int) -> (View -> View) -> a -> (st,View)
  }

articleStream :: Layout st a -> Stream.Stream IO View
articleStream Layout {..} = let ini = (initial,page,[],seed) in
  -- The chunk size of 4 guarantees that even on absurdly large screens that 
  -- show 2-up grids, there should be enough content to allow for triggering 
  -- the 400px stepper on scroll. 
  Stream.chunksOf chunk $ Stream.unfolds ini $ \(st,p,leftovers,seed) -> do

      -- The first page needs space for the masthead
      let bounds 
            | Null <- masthead 
            = (12,18)
            
            | p /= 0 
            = (12,18)

            | otherwise 
            = (12,16) 
      
      -- This approach takes advantage of the fact that the produced grids may
      -- not fully consume the articles returned from the server and avoids
      -- calling out unless absolutely required.
      let (seed',tiles) = generate (Pure.Autogrid.Layout.layout_ weights bounds) seed
          !ts = List.length tiles

      -- this assumes that `length tiles` is less than the length of whatever f
      -- returns.
      xs <- 
        if ts >= length leftovers
          then (leftovers ++) <$> request p
          else pure leftovers

      let
        (used,unused) = List.splitAt ts xs

        (st',page) =
          let
            (st',fs) = 
              List.mapAccumL 
                (\st ((start0,span),a) -> 
                  let 
                    start
                      | p == 0    = ((+2) (fst start0),snd start0)
                      | otherwise = start0
                    (st',v) = render st span (Pure.Autogrid.Layout.feature tile start span) a
                  in
                    (st',v)
                ) st (List.zip tiles used)
          in
            (st',Div <| Themed @(Grid 12 18 10) |>
              case p of
                0 -> Div <| Themed @(Tile 12 2) . extent 1 13 1 3 |> [ masthead ] : fs
                _ -> fs
            )

      if List.null xs 
        then Stream.done 
        else Stream.more page (st',succ p,unused,seed')

feature :: ((Int,Int) -> View -> View) -> (Int,Int) -> (Int,Int) -> View -> View
feature f (rs,cs) (w,h) a = a <| f (w,h) . ext
  where
    {-# NOINLINE i #-}
    i = toTxt :: Int -> Txt

    ext = extent (i (cs + 1)) (i (cs + w + 1)) (i (rs + 1)) (i (rs + h + 1))


{-# INLINE layout_ #-}
layout_ :: V.Vector Double -> (Int,Int) -> Generator [((Int,Int),(Int,Int))]
layout_ weights bounds = partition bounds gen
  where
    -- This simple approach seems to produce reasonably pleasant
    -- tilings without too much fine-tuning. 
    --
    -- This approach results in only 20 possible tile sizes, a
    -- modest, but reasonable number; the features embedded in 
    -- the tiles can, themselves, can be further randomized.

    gen :: (Int,Int) -> (Int,Int) -> Generator (Int,Int)
    gen (!mnw,!mxw) (!mnh,!mxh) =
      let 
        -- Avoid leaving space for a single column when possible.
        trim :: [Int] -> [Int]
        -- trim [2,4,6,10,12] = [2,4,6,12]
        -- trim (2:xs@(_:_)) = xs
        trim xs = xs

        grab :: Int -> Int -> [Int] -> Generator Int
        grab mn mx xs = do
          let ts = trim [ x | x <- xs, x >= mn, x <= mx ]
          n <- categorical (V.take (length ts) weights)
          pure (ts !! n)
      in 
        (,) <$> grab mnw mxw [2,4,6,10,12]
            <*> grab mnh mxh [2,4,6,8]

{-# INLINE pick #-}
pick :: V.Vector (Double,a) -> Generator a
pick (V.unzip -> (weights,options)) = do
  i <- categorical weights
  pure (options V.! i)

{-# INLINE partition #-}
-- For a 12x18 grid, this runs in ~100us in the browser vs 6us natively
-- 1e8 runs:
--   * mean    = 8.776 tiles
--   * σ       = 2.755 tiles
--   * minimum = 3 tiles
--   * maximum = 23 tiles
partition 
  :: forall state
   . (state ~ V.Vector (Int,V.Vector Int)) -- algorithm's internal state type - used in where clause
  => (Int,Int) 
  -> ((Int,Int) -> (Int,Int) -> Generator (Int,Int)) 
  -> Generator [((Int,Int),(Int,Int))]
partition (maxw,maxh) gen
  | maxw == 0 || maxh == 0 = pure []
  | otherwise = go empty
  where
    {-# NOINLINE empty #-}
    empty :: state
    empty = 
      V.fromList 
        [ (y,xs) 
        | y <- [0..maxh - 1]
        , let xs = V.fromList [0..maxw - 1]
        ]

    go :: state -> Generator [((Int,Int),(Int,Int))]
    go !st
      | V.null st = pure []
      | (!ly,!lx,!rx) <- bounds st = do 
        (!w,!h) <- gen (1,rx - lx + 1) (1,maxh - ly)
        ps <- go (excise lx w h st)
        pure (((ly,lx),(w,h)) : ps)

    {-# INLINE bounds #-}
    bounds :: state -> (Int,Int,Int)
    bounds st = 
      let 
        (ly,r) = V.unsafeHead st
        lx = V.unsafeHead r
        rx = V.foldr span id r lx
          where
            span new cont old 
              | new > old + 1 = old
              | otherwise     = cont new
       in 
         (ly,lx,rx)
         
    {-# INLINE excise #-}
    excise :: Int -> Int -> Int -> state -> state
    excise lx w h st = 
      let 
        (dirty,clean) = V.splitAt h st
        !cleaned = V.mapMaybe reduce dirty
          where
            reduce (i,v) = 
              let v' = V.filter (\x -> x < lx || x >= lx + w) v
              in if V.null v' then Nothing else Just (i,v')
      in 
        cleaned <> clean

data Container
instance Theme Container where
  theme c =
    is c . child (tag Div) $ do
      display           =: flex
      flex-direction    =: row
      flex-wrap         =: wrap
      justify-"content" =: center
      flex              =* [0,0,(50%)]

data Grid (width :: Nat) (height :: Nat) (pad :: Nat)
instance (KnownNat width, KnownNat height, KnownNat pad) => Theme (Grid width height pad) where
  theme c = do
    is c do
      let w = fromIntegral $ natVal (Proxy :: Proxy width)
          h = fromIntegral $ natVal (Proxy :: Proxy height)
          p = fromIntegral $ natVal (Proxy :: Proxy pad)

      width                 =: (100%)
      max-height            =: h px
      max-width             =: w px
      display               =: grid
      margin-top            =: p px
      row-gap               =: p px
      column-gap            =: p px
      grid-auto-rows        =: 1fr
      grid-auto-columns     =: 1fr
      grid-template-columns =: none
      padding-left          =: calc(p px / 2)
      padding-right         =: calc(p px / 2)

      atMedia ("only screen and (min-width:" <> w "px)") do
        width               =: w px

{-# INLINE extent #-}
extent :: Txt -> Txt -> Txt -> Txt -> (View -> View)
extent columnStart columnEnd rowStart rowEnd = 
  let 
    infixr </>
    (</>) x y = x <> " / " <> y
  in 
    Style (grid-area) 
      (rowStart </> columnStart </> rowEnd </> columnEnd)

