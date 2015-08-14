{-# LANGUAGE Arrows #-}
module Env where
	
import FRP.Yampa

import CLTypes

testster :: SF () (Amp, Amp)
testster = constant 0.5 >>> (identity &&& identity)

mono2stereo :: SF (Amp) (Amp,Amp)
mono2stereo = identity &&& identity


