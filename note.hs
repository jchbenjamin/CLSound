{-# LANGUAGE Arrows #-}
module Note where

import CLTypes

data Key = A | Aq | Bb | Bbq | B | Bq | C | Cq | Db | Dbq | D | Dq | Eb | Ebq | E | Eq | F | Fq | Gb | Gbq | G | Gq | Ab | Abq deriving Eq
type Oct = Int
type Cent = Int
type Note = (Key, Oct)
type Notec = (Key, Oct, Cent)

cent :: Cent -> Rtype
cent h
      | h <= 0x0 = 0.0
      | h > 0xf = 0.0
      | otherwise = 0.5 * ((fromIntegral h) / 0x10)

k2n :: Key -> Rtype
k2n k
      | k == A = 0.0
      | k == Aq = 0.5
      | k == Bb = 1.0
      | k == Bbq = 1.5
      | k == B = 2.0
      | k == Bq = 2.5
      | k == C = -9.0
      | k == Cq = -8.5
      | k == Db = -8.0
      | k == Dbq = -7.5
      | k == D = -7.0
      | k == Dq = -6.5
      | k == Eb = -6.0
      | k == Ebq = -5.5
      | k == E = -5.0
      | k == Eq = -4.5
      | k == F = -4.0
      | k == Fq = -3.5
      | k == Gb = -3.0
      | k == Gbq = -2.5
      | k == G = -2.0
      | k == Gq = -1.5
      | k == Ab = -1.0
      | k == Abq = -0.5

note :: Note -> Hz
note (k0, o) =
      let
            n = (k2n k0) + (12 * ( (fromIntegral o) - 4 ))
      in 2 ** ( n / 12) * 440

notec :: Notec -> Hz
notec (k0, o, h) =
      let
            n = (k2n k0) + (cent h) + (12 * ( (fromIntegral o) - 4 ))
      in 2 ** ( n / 12) * 440
