#!/usr/bin/env stack
{- stack runghc
    --package base
    --package diagrams-lib
    --package diagrams-svg
    --package diagrams-rasterific
-}

    -- -hide-all-packages
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

import System.Environment
import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Rasterific.CmdLine

paramsNArgs = [("a", "x"), ("b", "y"), ("c", "z")]

main = mainWith [("values", funcArgCombo paramsNArgs 1)]

-- max arity, current arity
funcArgCombo p 0 = funcbox (map fst p) 0
funcArgCombo p n = funcbox (map fst p) n <> argApply (map snd p) n

funcbox :: [String] -> Int -> Diagram B
funcbox params n = (strokeLocTrail $ tr `at` p2 (0, 0)) <> plabels
    where tr = (closeTrail . trailFromVertices) (map p2 pts)
          pts = [(0,0), (0, -1), (1, -1), (1, 0)] ++ argPts
          plabels :: Diagram B
          plabels = atPoints (map p2 argTips) (map ((scale 0.1) . topLeftText) (reverse params))
          argTips :: [(Double, Double)]
          argTips = map snd $ filter (even . fst) (zip [1..] argPts)
          argPts = take (2 * n + 1)
                   $ zip (map ((* (base / 2)) . toFrac) lst)
                         (map (\x -> if odd x then (- height) else 0.0) lst)
                    where lst = reverse [1..2 * m]
                          (base, height) = triangleProps m
                          m = length params

toFrac = fromRational . toRational

-- the triangle used to represent one argument
-- (base, height)
triangleProps m = (base, (base / 2) * sqrt 3)
    where base = 1 / (toFrac m)

argApply :: [String] -> Int -> Diagram B
argApply args n =
    (triangle base # reflectY <> text (args !! (m - n)) # scale 0.1)
    # moveTo (p2 ((toFrac (m - n) * base + base / 2), 0))
    where (base, _) = triangleProps m
          m = length args
