module Main where

import Control.Monad.State
import Graphics.Implicit
import Graphics.Implicit.Definitions

polyDots a n =
    let
        angle i = fromIntegral i * 2 * pi / fromIntegral n
        dot alpha = (a * cos alpha, a * sin alpha)
    in
        map (dot . angle) [0..n-1]

handle len r1 r2 = extrudeRM 0 twist scale Nothing base (Left len)
    where
        base = polygonR 0 (polyDots r1 7)
        twist = Just $ \x ->
            let y = x/len
            in 120 * (y/2 + y*y*2)
        scale = Just $ \x ->
            let y = x/len
                wedge = (1 + (r2 - r1) * y/r1)
                shape = (2 * y * (y - 1) + 1)
            in wedge * shape

tamper w r = extrudeR 0 (circle r) w

scoop r = difference [sphere r, rect3R 0 (0, -r, -r) (r, r, r)]

model = flip execState (sphere 1) $ do -- spere is dummy
--  handle
    modify' $ const $ translate (0, 0, tW - epsilon) $ handle hL (tR * 0.93) hW
--  tamper
    modify' $ \s -> unionR 1.6 [s, tamper tW tR]
--  remove rounding artifacts around taper
    modify' $ \s -> intersect [s, cylinder tR (hL + tW)]
--  scoop
    modify' $ \s -> unionR 6 [s, translate scoopPos $ scoop sR]
--  inner volume or the scoop, separate to avoid handle protruding inside.
    modify' $ \s -> difference [s, translate scoopPos $ sphere (sR - sW)]
  where
    epsilon = 0.001
    tW = 1
    tR = 10
    hL = 30
    hW = 2
    sR = 8
    sW = 1
    scoopPos = (sR / 2.35, 0, hL + 0.9 * sR)

main :: IO ()
main = do
    writeSTL 0.2 "./result.stl" model
