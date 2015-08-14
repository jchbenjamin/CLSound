{-# LANGUAGE Arrows #-}
module Synth where
	
import FRP.Yampa

import CLTypes
import Note

flatLine :: SF () (Amp)
flatLine = constant 0.0

offLine :: Rtype -> SF () (Amp)
offLine i = constant i

--this is dumb does nothing
whattest :: SF () (Amp)
whattest = offLine 0.5 >>> oscSine (note (A, 4)) 

sineGen :: Hz -> SF () (Amp)
sineGen f = constant(2 * pi * f) >>> integral >>^ (sin)

oscSine :: Hz -> SF (Amp) (Amp)
oscSine f = proc i -> do
      phi <- integral -< (2 * pi * f)
      returnA -< ((sin phi) + i)

oscSine' :: Hz -> Hz -> SF (Amp) (Amp)
oscSine' f s= proc i -> do
      phi <- integral -< (2 * pi * f)
      returnA -< ((sin (phi - (2*pi /s) )) + i)

oscTri :: Amp -> Hz -> SF () (Amp)
oscTri t0 f = switch (lf t0 f) (\(a,f) -> oscTri a (-f) )
      where lf t' f' = proc i -> do
            a <- line t' f' -< i
            event <- edge -< (abs a) >= 1.0
            returnA -< (a, event `tag` (a,f'))

line :: Amp -> Hz -> SF () (Amp)
line t0 f = proc i -> do
      phi <- integral -< f * 4
      returnA -< (phi) + t0

--envelope
volAdjust :: Amp -> SF (Amp) (Amp) -> SF (Amp) (Amp)
volAdjust v s
      | v <= 0.0 = constant 0.0
      | v > 1.0 = s >>> identity
      | otherwise = s >>^ (*v)

tritest = oscTri 0 (note (A,4))
--additive
tunesine :: Notec -> Rtype -> SF (Amp) (Amp)
tunesine n v = volAdjust v (oscSine (notec n)) 

addSynth :: [SF (Amp) (Amp)] -> SF () (Amp)
addSynth [] = flatLine
addSynth (x:xs) = x <<< addSynth xs

ta = tunesine (A, 4, 0) 0.9
tb = tunesine (B, 4, 0) 0.8
tc = tunesine (C, 5, 0) 0.7
td = tunesine (D, 5, 0) 0.6

addtest = addSynth [ta,tb,tc,td]

ba = tunesine (A, 1, 0) 0.9
bb = tunesine (C, 2, 0) 0.3
bc = tunesine (D, 2, 0) 0.2

bass = addSynth [ba,bb,bc]

myChain :: SF () (Amp)
myChain = flatLine >>> oscSine (note (A, 4)) >>> volAdjust 7.0 (oscSine (note (Db, 5))) >>> volAdjust 3.0 (oscSine (note (E, 5)))
