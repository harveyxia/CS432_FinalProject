> {-# LANGUAGE Arrows #-}
> module Final where
> import Euterpea
> import Control.Arrow ((>>>), (<<<), arr)
> import Data.Complex

Final project sound effects

> sinTab :: Table
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


> ramp1 :: AudSF () Double
> ramp1 = proc afrq -> do
>   env <- envLine 20 10 20000 -< ()
>   sig <- osc sinTab 0 -< env
>   outA -< sig

SOURCES:
- http://en.wikipedia.org/wiki/Phaser_(effect)
- https://ccrma.stanford.edu/~jos/pasp/Allpass_Two_Combs.html
- https://ccrma.stanford.edu/~jos/pasp/Phasing_First_Order_Allpass_Filters.html
- https://ccrma.stanford.edu/~jos/pasp/Schroeder_Reverberators.html
- https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/processing2.html
- http://en.wikipedia.org/wiki/Distortion_(music)

================================================================================
Phaser effects
Uses phase shifting to create an interesting undulating sound
================================================================================

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
>       s <- violin (absPitch (A, 5)) -< ()
>       f <- phaser 0.05 0.15 0.6 0.6 1 -< s
>       outA -< f / 5

> testPhaser = outFile "phaser.wav" 10 tPhaser

================================================================================

A fuzzbox effect that clips off the peaks of a signal to produce a distorted,
"dirty" electric guitar sound.

dep = depth of distortion

> fuzzbox :: Double -> AudSF Double Double
> fuzzbox dep =
>   proc s -> do
>       outA -< if (abs (s*(1+dep))) > 1 then (if s < 0 then (-1) else 1) else s

> vibrato :: Clock c => Double -> Double -> SigFun c Double Double
> vibrato vfrq dep = proc afrq -> do
>               vib <- osc sinTab 0 -< vfrq
>               aud <- osc sinTab 0 -< afrq + vib * dep
>               outA -< aud

> electro :: Instr (Mono AudRate)
> electro dur ap vol [] =
>     let f    = apToHz ap
>         v    = fromIntegral vol / 100
>         fund = (constA f >>> osc sinTab 0)
>         ots  = (map (\p -> constA (f * p) >>> vibrato 18 10)
>                  [2, 3, 4])
>         sfs  = fund : ots
>         sig  = foldSF (+) 0 sfs
>     in proc () -> do
>         a <- sig -< ()
>         outA -< a * v

> testFB = outFile "fuzzbox.wav" 5 ((fuzzbox 0.7) <<< (electro 10 35 20 []))

================================================================================

This reverb implementation is based on Manfred Shroeder's model. A signal is
first passed through a parallel bank of feedback comb filters.  The output of
the comb filters are added and fed into a series of three all-pass filters.
Delay values of these all-pass filters are mutually prime to prevent any mutual
periodicity between their outputs.

> allPassSection :: AudSF Double Double
> allPassSection = proc s -> do
>       s1 <- filterAllPass 0.100 0.100 0 0.7 -< s
>       s2 <- filterAllPass 0.033 0.033 0 0.7 -< s1
>       s3 <- filterAllPass 0.011 0.011 0 0.7 -< s2
>       outA -< s3

> feedbackFilter :: Double -> Double -> Double -> AudSF Double Double
> feedbackFilter del a b = proc s -> do
>       rec d <- delayLine del -< s + (-a)*d
>       outA -< b*d

> schroederRev :: AudSF Double Double
> schroederRev = proc s -> do
>       c1 <- feedbackFilter 0.033 0.773 1 -< s
>       c2 <- feedbackFilter 0.037 0.802 1 -< s
>       c3 <- feedbackFilter 0.041 0.753 1 -< s
>       c4 <- feedbackFilter 0.045 0.733 1 -< s
>       f <- allPassSection -< (c1 + c2 + c3 + c4)
>       outA -< f

Test with a short note.

> tRev :: AudSF () Double
> tRev = proc () -> do
>       env <- envLineSeg [1,1,0,0] [0.05,0.05,5] -< ()
>       s <- violin (absPitch (C, 5)) -< ()
>       f <- schroederRev -< s*env
>       outA -< f

> testRev = outFile "shroeder.wav" 5 tRev

================================================================================

Welcome to R&D.

> myscifi1 :: Instr (Mono AudRate)
> myscifi1 dur ap vol [] =
>   let v = fromIntegral vol / 100 in proc () -> do
>       a1   <- noiseBLH 42 -< 3
>       a2   <- osc sinTab 0 -< [440, 466.164, 493.883, 523.251, 554.365, 587.330, 622.254, 659.255, 698.456, 739.989, 783.991, 830.609]!!(round ((a1 ^ 2) * 11))
>       outA -< a2 * v

> test3 = outFile "scifi.wav" 10 (schroederRev <<< (myscifi1 10 (absPitch (C, 5)) 100 []))