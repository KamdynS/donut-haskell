module Main where

import System.IO
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay)

-- Constants for the torus dimensions
thetaSpacing, phiSpacing :: Float
thetaSpacing = 0.07
phiSpacing = 0.02

screenWidth, screenHeight :: Int
screenWidth = 60
screenHeight = 60

r1, r2 :: Float
r1 = 0.5
r2 = 1.5

k2, k1 :: Float
k2 = 5
k1 = fromIntegral screenWidth * k2 * 3 / (8 * (r1 + r2))

type Point = (Int, Int, Float, Char)

-- Generate a single point
generatePoint :: Float -> Float -> Float -> Float -> Maybe Point
generatePoint a b theta phi =
    let
        (cosA, cosB, sinA, sinB) = (cos a, cos b, sin a, sin b)
        (cosTheta, sinTheta) = (cos theta, sin theta)
        (cosPhi, sinPhi) = (cos phi, sin phi)
        circleX = r2 + r1 * cosTheta
        circleY = r1 * sinTheta
        x = circleX * (cosB * cosPhi + sinA * sinB * sinPhi) - circleY * cosA * sinB
        y = circleX * (sinB * cosPhi - sinA * cosB * sinPhi) + circleY * cosA * cosB
        z = k2 + cosA * circleX * sinPhi + circleY * sinA
        ooz = 1/z
        xp = round $ fromIntegral screenWidth / 2 + k1 * ooz * x
        yp = round $ fromIntegral screenHeight / 2 - k1 * ooz * y
        l = cosPhi * cosTheta * sinB - cosA * cosTheta * sinPhi - sinA * sinTheta + cosB * (cosA * sinTheta - cosTheta * sinA * sinPhi)
        luminanceIndex = round (l * 8)
        luminanceChar = ".,-~:;=!*#$@" !! luminanceIndex
    in
        if l > 0
        then Just (xp, yp, ooz, luminanceChar)
        else Nothing

-- Render a single frame
renderFrame :: Float -> Float -> [[Char]]
renderFrame a b =
    let
        points = [ p | theta <- [0, thetaSpacing .. 2*pi], 
                       phi <- [0, phiSpacing .. 2*pi], 
                       Just p <- [generatePoint a b theta phi] ]
        finalMap = foldl' updatePoint M.empty points
        updatePoint acc (x, y, ooz, ch) =
            M.insertWith (\new old -> if fst new > fst old then new else old) (x, y) (ooz, ch) acc
        renderChar x y = maybe ' ' snd (M.lookup (x, y) finalMap)
    in
        [ [ renderChar x y | x <- [0..screenWidth-1] ] | y <- [0..screenHeight-1] ]

-- Set cursor position using ANSI escape code
setCursorPosition :: Int -> Int -> IO ()
setCursorPosition row col = putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"

-- Update screen with changes
updateScreen :: [[Char]] -> [[Char]] -> IO ()
updateScreen old new = do
    forM_ (zip [0..] (zip old new)) $ \(y, (oldRow, newRow)) -> do
        forM_ (zip [0..] (zip oldRow newRow)) $ \(x, (oldChar, newChar)) -> do
            when (oldChar /= newChar) $ do
                setCursorPosition y x
                putChar newChar
    hFlush stdout

-- Main function with animation loop
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStr "\ESC[2J"  -- Clear screen once at the start
    setCursorPosition 0 0
    let loop a b oldFrame = do
        let newFrame = renderFrame a b
        updateScreen oldFrame newFrame
        threadDelay 50000  -- 50ms delay
        loop (a + 0.04) (b + 0.02) newFrame
    loop 0 0 (replicate screenHeight (replicate screenWidth ' '))