{-# LANGUAGE Arrows #-}
module Render where
	
import FRP.Yampa
import Data.WAVE
import Data.Int (Int32)
import Data.FixedPoint

import CLTypes

samplesPS = 44100 -- samples per second
bitrate = 32

monoheader = WAVEHeader 1 samplesPS bitrate Nothing
stereoheader = WAVEHeader 2 samplesPS bitrate Nothing

toInt32 :: Rtype -> Int32
toInt32 i 
      | i <= 1.0 && i >= -1.0 = round(i * fromIntegral(maxBound::Int32))
      | i > 1.0 = maxBound::Int32
      | i < -1.0 = -(maxBound::Int32)
      | otherwise = 0

embed2wavsamples :: Rtype -> Rtype -> SF () (Amp) -> WAVESamples
embed2wavsamples v t s = map (:[]) $ map (toInt32) $ master v $ embed (s) ((), (embedMk samplesPS t)) --inefficient

{-
embed2wavsamplesstereo :: Rtype -> Rtype -> SF () (Amp,Amp) -> WAVESamples
embed2wavsamplesstereo v t s = map (:[]) $ map (toInt32) $ master v $ embed (s) ((), (embedMk samplesPS t)) --inefficient
-}

master :: Rtype -> [Rtype] -> [Rtype] -- adjustvolume of raw signal before conversoin to Int32 , inefficient?
master v x = scaleList (v / factor) x
      where factor = maxabs x

maxabs :: [Rtype] -> Rtype
maxabs [] = error "maxabs on empty list"
maxabs [x] = abs x
maxabs (x:xs) = max (abs x) (maxabs xs)

scaleList :: Rtype -> [Rtype] -> [Rtype]
scaleList i [] = []
scaleList i (x:xs) = [i*x] ++ scaleList i xs

embedMk :: Int -> Rtype -> [(DTime, Maybe a)]
embedMk s l = -- s is samples per second, l is length in seconds
      let
            si = 1.0 / (fromIntegral s)
            n = round ((fromIntegral s) * l) :: Int
      in [(si, Nothing) | _ <- [1..n]]

makeWavFile :: String -> WAVE -> IO ()
makeWavFile s wav = putWAVEFile s wav
