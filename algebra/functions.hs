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
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Rasterific.CmdLine

--main = mainWith [("functions", invalid === (partial ||| strutX 1 ||| total))]
main = mainWith
    [ ("invalid", invalid)
    , ("partial", partial)
    , ("total"  , total)
    ]

namedText :: String -> Diagram B
namedText x = text x # named x

dom :: [String] -> Diagram B
dom xs = ellipse 0.92 <> (vsep 1.3 (map namedText xs) # scale 0.25 # centerY)

domain, codomain :: [String] -> Diagram B
domain xs = label "Domain" === dom xs
codomain xs = (label "Codomain" ||| strutX 0.52) === dom xs

label :: String -> Diagram B
label x =
    strutY 0.25
        ===
    text x # scale 0.2
        ===
    strutY 0.25

func :: String -> Diagram B -> Diagram B -> [(String, String)] -> Diagram B
func l d c a = frame 0.2 $
    foldr (uncurry connect) (hsep 1 [d, c]) a
    # centerX
    === label l

invalid :: Diagram B
invalid = func "Invalid"
    (domain ["a", "b", "c"])
    (codomain ["1", "2", "3"])
    [("a", "1"), ("b", "2"), ("b", "3")]

partial :: Diagram B
partial = func "Partial"
    (dom ["a", "b", "c"])
    (dom ["1", "2", ""])
    [("a", "1"), ("b", "2")]


total :: Diagram B
total = func "Total"
    (dom ["a", "b", "c"])
    (dom ["1", "2", "3"])
    [("a", "1"), ("b", "2"), ("c", "3")]
