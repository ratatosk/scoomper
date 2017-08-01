module Main where

import Graphics.Implicit
import Graphics.Implicit.Definitions

polyDots a n =
    let
        angle i = fromIntegral i * 2 * pi / fromIntegral n
        dot alpha = (a * cos alpha, a * sin alpha)
    in
        map (dot . angle) [0..n-1]

handle len r1 r2 = extrudeRM 0 twist (Just scale) Nothing base (Left len)
    where
        base = polygonR 0 (polyDots r1 6)
        twist = Just (*pi)
        scale x =
            let y = x/len
                wedge = (1 + (r2 - r1) * y/r1)
                shape = (y * (y - 1) + 0.5)
            in wedge * shape

tamper = extrudeR 0 (circle 5) 2

model = unionR 0.3 [translate (0, 0, 2.2) $ handle 20 10 3, tamper]

main :: IO ()
main = do
    writeSTL 0.1 "./result.stl" model
