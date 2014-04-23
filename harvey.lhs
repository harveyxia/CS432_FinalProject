> {-# LANGUAGE Arrows #-}
> import Euterpea
> import Control.Arrow ((>>>),(<<<),arr)
> import Control.SF.AuxFunctions

Sources:
- http://www.cs.sfu.ca/~tamaras/delayEffects/Implementation_Chorus.html

> sinTab = tableSinesN 4096 [1]

> violin :: AbsPitch -> AudSF () Double
> violin ap =
>       let f     = apToHz ap
>           sfs   = map (mySF f)
>                           [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
>       in proc () -> do
>           a <- foldSF (+) 0 sfs -< ()
>           outA -< a / 5

> mySF f p =
>       let amp x =
>               case x of
>                   1 -> 1
>                   2 -> 1.2
>                   3 -> 0.4
>                   4 -> 0.04
>                   5 -> 0.3
>                   6 -> 0.35
>                   7 -> 0.03
>                   8 -> 0.6
>                   9 -> 0.009
>                   10 -> 0.02
>                   11 -> 0.04
>                   _  -> 0
>       in proc () -> do
>           s <- osc sinTab 0 <<< constA (f*p) -< ()
>           outA -< s * (amp p)

=======================================================

dmin = minimum delay time in seconds
dmax = maximum delay time in seconds
freq = frequency of the low frequency modulator (for delay time)
depth = amplitude multiplier for flanged signal

This flanger also incorporates an echo of 0.05 seconds, decay rate 0.3, in
order to intensify the flanging effect.

If depth is set to a negative value, the flanger is in inverted mode.

> flanger :: Double -> Double -> Double -> Double -> AudSF Double Double
> flanger dmin dmax freq depth =
>       let middle = (dmin + dmax)/2
>           mod = dmax - middle
>       in proc s -> do
>           sin <- osc sinTab 0 -< freq
>           g <- delayLine1 1 -< (s, middle + (mod * sin))
>           rec f <- delayLine 0.05 -< s + (depth * g) + 0.3*f
>           outA -< f

> tFlanger :: AudSF () Double
> tFlanger = proc () -> do
>       s <- violin (absPitch (A, 5)) -< ()
>       f <- flanger 0.05 0.15 0.6 1 -< s
>       outA -< f/5

> testFlanger = outFile "flanger.wav" 5 tFlanger

================================================================================

freq = frequency of low frequency modulator
depth = amplitude coefficient for chorus effect

Chorus is implemented with 4 delay lines, each delaying the original signal
by a time-varying amount (between 20 and 50 ms), according to sin waves that are
each offset by a different phase.

> chorus :: Double -> Double -> AudSF Double Double
> chorus freq depth = proc s -> do
>           sin1 <- osc sinTab 0 -< freq
>           sin2 <- osc sinTab 0.25 -< freq
>           sin3 <- osc sinTab 0.5 -< freq
>           sin4 <- osc sinTab 0.75 -< freq
>           c1 <- delayLine1 1 -< (s, 0.024 + 0.001*sin1)
>           c2 <- delayLine1 1 -< (s, 0.033 + 0.001*sin2)
>           c3 <- delayLine1 1 -< (s, 0.047 + 0.001*sin3)
>           c4 <- delayLine1 1 -< (s, 0.036 + 0.001*sin4)
>           outA -< (c1 + c2 + c3 + c4)*depth + s

> tChorus :: AudSF () Double
> tChorus = proc () -> do
>       s <- violin (absPitch (A, 5)) -< ()
>       f <- chorus 0.6 1 -< s
>       outA -< f/5

> testChorus = outFile "chorus.wav" 5 tChorus