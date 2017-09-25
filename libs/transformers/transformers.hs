#!/usr/bin/env stack
{- stack runghc
    --package base
    --package diagrams-lib
    --package diagrams-svg
    --package diagrams-rasterific
    --
    -hide-all-packages
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import System.Environment
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Data.Maybe (fromMaybe)
import Control.Arrow (second)

main = mainWith $ map (second (frame 0.2))
    [ ("monad"                  , monad "m")
    , ("transformer"            , transformer)
    , ("transformer-stack2"     , transformerStack2)
    , ("transformer-lift"       , transformerLift)
    , ("transformer-lift2"      , transformerLift2)
    , ("transformer-base2"      , transformerBase2)
    , ("transformer-base-lift2" , transformerBaseLift2)
    , ("transformer-io-lift2"   , transformerIOLift2)
    ]

monadLift :: String -> Diagram B
monadLift label =
    monad label <> arrowAt (p2 (0, -1)) unitY

addLift :: String -> String -> Point V2 Double -> Double -> Diagram B
addLift n1 n2 p len =
    arrowAt p (unitY # scale len)
    <> alignedText (-0.15) 1 ("lift " ++ n1)
        # rotate (1/4 @@ turn)
        # scale (len/4)
        # moveTo p
        # moveOriginBy (V2 0.2 0)
    <> (origin ~~ p2 (1, 0)
        ||| alignedText 0 0.5 ("MonadTrans " ++ n2) # scale 0.1
        ||| strutX 0.75)
       # moveTo (p + p2 (0, len))

addBaseLift :: String -> String -> String -> Point V2 Double -> Double -> Angle Double -> Diagram B
addBaseLift tclass lift to p len ang =
    (arrowAt p (unitY # scale len)
    <> alignedText (-0.15) 1 (lift)
        # rotate (1/4 @@ turn)
        # scale (len/8)
        # moveTo p
        # moveOriginBy (V2 0.2 0)) # rotate ang
    <> (origin ~~ p2 (1, 0)
        ||| alignedText 0 0.5 (tclass ++ to) # scale 0.1
        ||| strutX 0.8)
       # moveTo (p + p2 (0, len))

transformerLift :: Diagram B
transformerLift =
    transformer
    <> addLift "m" "t" (p2 (0, 1/2)) (1/2)

transformerBaseLiftCommon :: String -> String -> String -> Diagram B
transformerBaseLiftCommon tclass var lift =
    transformerBase2
    <> addBaseLift tclass' lift "t2"  (p2 (0, 1/3)) (1/3) (-1/16 @@ turn)
    <> addBaseLift tclass' lift "t1" (p2 (0, 1/3)) (2/3) (1/16 @@ turn)
    <> (origin ~~ p2 (1, 0)
        ||| alignedText 0 0.5 (tclass' ++ var) # scale 0.1
        ||| strutX 0.8)
       # moveTo (p2 (0, 1/3))
    where tclass' = tclass ++ " "

transformerBaseLift2 :: Diagram B
transformerBaseLift2 = transformerBaseLiftCommon "MonadBase b" "b" "liftBase b"

transformerIOLift2 :: Diagram B
transformerIOLift2 = transformerBaseLiftCommon "MonadIO" "IO" "liftIO"

transformerLift2 :: Diagram B
transformerLift2 =
    transformerStack2
    <> addLift "m" "t2"  (p2 (0, 1/3)) (1/3)
    <> addLift "t2" "t1" (p2 (0, 2/3)) (1/3)

transformerBase2 :: Diagram B
transformerBase2 =
       monad "t1"
    <> monad "t2" # scale (2 / 3)
    <> monad "b"  # scale (1 / 3)

transformerStack2 :: Diagram B
transformerStack2 =
       monad "t1"
    <> monad "t2" # scale (2 / 3)
    <> monad "m"  # scale (1 / 3)

transformer :: Diagram B
transformer =
       monad "t"
    <> monad "m" # scale (1 / 2)

monad :: String -> Diagram B
monad label =
    arc xDir (1 / 2 @@ turn) # named label
    <> mkLabel unitX
    <> mkLabel unit_X
    where
        mkLabel :: V2 Double -> Diagram B
        mkLabel l = text label # scale 0.2 # moveOriginBy l
