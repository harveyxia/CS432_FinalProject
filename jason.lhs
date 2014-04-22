> {-# LANGUAGE Arrows #-}
> module Final where
> import Euterpea
> import Control.Arrow ((>>>), (<<<), arr)
> import Data.Complex

Final project sound effects

> sinTab :: Table
> sinTab = tableSinesN 4096 [1]

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

> filterAllPass :: Double -> Double -> AudSF Double Double
> filterAllPass dur ga =
>   proc s -> do
>       rec out1 <- delayLine dur -< s + out1 * ga
>       outA -< (out1 - (s + out1 * ga) * ga)

> phaser :: Double -> Double -> Double -> AudSF Double Double
> phaser dur dep ga =
>   proc s -> do
>       out1 <- filterAllPass dur ga -< s
>       outA -< (s + out1 * dep) / 5

> testSig :: AudSF () Double
> testSig =
>   proc () -> do
>       s <- osc sinTab 0 -< 440
>       outA -< s

> test1 = outFile "phaser.wav" 10 ((filterComb 0.2) <<< ((constA 440) ramp1))

