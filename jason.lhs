> {-# LANGUAGE Arrows #-}
> module Final where
> import Euterpea
> import Control.Arrow ((>>>), (<<<), arr)
> import Data.Complex

Final project sound effects

> sinTab :: Table
> sinTab = tableSinesN 4096 [1]

SOURCES:
- http://en.wikipedia.org/wiki/Phaser_(effect)
- https://ccrma.stanford.edu/~jos/pasp/Allpass_Two_Combs.html
- https://ccrma.stanford.edu/~jos/pasp/Phasing_First_Order_Allpass_Filters.html

================================================================================
Phaser effects
Uses phase shifting to create an interesting undulating sound
================================================================================

> ramp1 :: AudSF () Double
> ramp1 = proc afrq -> do
>   env <- envLine 20 10 20000 -< ()
>   sig <- osc sinTab 0 -< env
>   outA -< sig

--> filterComb :: Double -> Double -> AudSF Double Double
--> filterComb dur ga =
-->   proc s -> do
-->       out1 <- delayLine dur -< s
-->       outA -< s  + out1 * ga


An all-pass filter that shifts the phase of the signal by a changing amount,
determined by the frequency of a sinusoidal modulator signal.

dmin = minimum delay time in seconds
dmax = maximum delay time in seconds
freq = frequency of the low frequency modulator (for delay time)
g = feedforward and feedbackward coefficient

> filterAllPass :: Double -> Double -> Double -> Double -> AudSF Double Double
> filterAllPass dmin dmax freq g =
>       let middle = (dmin + dmax)/2
>           mod = dmax - middle
>       in proc s -> do
>           sin <- osc sinTab 0 -< freq
>           rec out1 <- delayLine1 1 -< (s + out1 * g, middle + (mod * sin))
>           outA -< (out1 * (-g)) + out1

This phaser is implemented with 4 all pass filters to intensify the sweep-effect.

> phaser :: Double -> Double -> Double -> Double -> Double -> AudSF Double Double
> phaser dmin dmax freq g depth =
>   proc s -> do
>       out1 <- filterAllPass dmin dmax freq g -< s
>       out2 <- filterAllPass dmin dmax freq g -< out1
>       out3 <- filterAllPass dmin dmax freq g -< out2
>       out4 <- filterAllPass dmin dmax freq g -< out3
>       outA -< (s + (out4 * depth))


> tPhaser :: AudSF () Double
> tPhaser = proc () -> do
>       s <- osc sinTab 0 -< 440
>       f <- phaser 0.05 0.15 0.6 0.6 1 -< s
>       outA -< f / 5

> testPhaser = outFile "phaser.wav" 10 tPhaser