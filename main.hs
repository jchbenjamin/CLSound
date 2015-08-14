{-# LANGUAGE Arrows #-}
module CLSound where

import FRP.Yampa
import Control.Concurrent
import Data.WAVE

import CLTypes
import Render
import Synth
import Env


filename = "test.wav"

main :: IO ()
main = do
	putStrLn "CLSound baby"
	testwav filename
	putStrLn $ "Created file: " ++ filename ++ " !!"
	putStrLn "Done"


testwav :: String -> IO ()
testwav s = makeWavFile s waveData

waveData = WAVE monoheader (embed2wavsamples 0.9 4 bass)
--pi/1760
action :: SF () (Amp) -> IO ()
action s = reactimate (return ())
		(\ _ -> threadDelay 100000 >> return ((0.1), Nothing))
		(\ _ (a) -> putStrLn ("amp: " ++ show a ) >> return False)
		(s)

