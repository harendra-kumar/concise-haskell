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

import System.Environment
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

main = mainWith
    [ ("monad", monad "m")
    , ("transformer", transformer)
    , ("transformer-stack2", transformerStack2)
    ]

transformer :: Diagram B
transformer =
       monad "t"
    <> monad "m" # scale 0.5

transformerStack2 :: Diagram B
transformerStack2 =
       monad "t1"
    <> monad "t2" # scale (2 / 3)
    <> monad "m"  # scale (1 / 3)

monad :: String -> Diagram B
monad label = frame 0.2 $
    arc xDir (1 / 2 @@ turn)
    <> mkLabel unitX
    <> mkLabel unit_X
    where
        mkLabel :: V2 Double -> Diagram B
        mkLabel l = text label # scale 0.2 # moveOriginBy l
