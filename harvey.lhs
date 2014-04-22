> {-# LANGUAGE Arrows #-}
> import Euterpea
> import Control.Arrow ((>>>),(<<<),arr)
> import Control.SF.AuxFunctions

> sinTab = tableSinesN 4096 [1]

=======================================================

dmin = minimum delay time in seconds
dmax = maximum delay time in seconds
freq = frequency of the low frequency modulator (for delay time)
depth = amplitude multiplier for flanged signal

This flanger also incorporates a reverb of 0.05 seconds, decay rate 0.3, in
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
>       s <- osc sinTab 0 -< 440
>       f <- flanger 0.05 0.15 0.6 0.6 -< s
>       outA -< f/5

> testFlanger = outFile "flanger.wav" 5 tFlanger