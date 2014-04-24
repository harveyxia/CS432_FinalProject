> {-# LANGUAGE Arrows #-}
> module Final where
> import Euterpea
> import Control.Arrow ((>>>), (<<<), arr)
> import Data.Complex

================================================================================
SOURCES
================================================================================
- https://ccrma.stanford.edu/~jos/pasp/Flanging.html
- http://en.wikipedia.org/wiki/Phaser_(effect)
- https://ccrma.stanford.edu/~jos/pasp/Allpass_Two_Combs.html
- https://ccrma.stanford.edu/~jos/pasp/Phasing_First_Order_Allpass_Filters.html
- https://ccrma.stanford.edu/~jos/pasp/Schroeder_Reverberators.html
- https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/processing2.html
- http://en.wikipedia.org/wiki/Distortion_(music)
- http://www.cs.sfu.ca/~tamaras/delayEffects/Implementation_Chorus.html

================================================================================
Utility Code
================================================================================

> sinTab :: Table
> sinTab = tableSinesN 4096 [1]

> clarinet :: Instr (Mono AudRate)
> clarinet dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>           sfs   = map (mySF f d)
>                           [1, 3, 5, 7, 9, 11, 13]
>       in proc () -> do
>           env <- envASR (d/8) d (d/5) -< ()
>           a <- foldSF (+) 0 sfs -< ()
>           outA -< a * env * v / 3

> mySF f d p =
>       let amp x =
>               case x of
>                   1 -> 1
>                   3 -> 0.75
>                   5 -> 0.5
>                   7 -> 0.14
>                   9 -> 0.5
>                   11 -> 0.12
>                   13 -> 0.17
>                   _  -> 0
>       in proc () -> do
>           s <- osc sinTab 0 <<< constA (f*p) -< ()
>           outA -< s * (amp p)


> ramp1 :: AudSF () Double
> ramp1 = proc afrq -> do
>   env <- envLine 20 10 20000 -< ()
>   sig <- osc sinTab 0 -< env
>   outA -< sig

> fuse       :: [Dur] -> [Dur -> Music a] -> [Music a]
> fuse (d:ds) (n:ns) = (n d) : fuse ds ns
> fuse [] []         = []

toS is a function that converts a delay time in terms of table size to its
corresponding delay in terms of seconds. In other words, toS outputs the seconds
that it would take to sample through a table of size s at a rate of 44.1 kHz.

> toS :: Double -> Double
> toS s = (s / 44100)

================================================================================
Phaser
================================================================================

dmin = minimum delay time in seconds
dmax = maximum delay time in seconds
freq = frequency of the low frequency modulator (for delay time)
depth = amplitude multiplier for flanged signal

If depth is set to a negative value, the flanger is in inverted mode.

> flanger :: Double -> Double -> Double -> Double -> AudSF Double Double
> flanger dmin dmax freq depth =
>       let middle = (dmin + dmax)/2
>           mod = dmax - middle
>       in proc s -> do
>           sin <- oscI sinTab 0 -< freq
>           rec g <- delayLine1 1 -< (s + 0.4*g, middle + (mod * sin))
>           outA -< depth*g + s

> tFlanger :: AudSF () Double
> tFlanger = proc () -> do
>       s <- clarinet 5 (absPitch (A, 5)) 3 [] -< ()
>       f <- flanger 0.006 0.020 1 0.5 -< s
>       outA -< f/5

> testFlanger = outFile "flanger.wav" 5 tFlanger

================================================================================
Chorus
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
>       s <- clarinet 5 (absPitch (A, 5)) 3 [] -< ()
>       f <- chorus 0.6 1 -< s
>       outA -< f/5

> testChorus = outFile "chorus.wav" 5 tChorus

================================================================================
Phaser
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
>       s <- clarinet 5 (absPitch (A, 5)) 3 [] -< ()
>       f <- phaser 0.05 0.15 0.6 0.6 1 -< s
>       outA -< f / 5

> testPhaser = outFile "phaser.wav" 10 tPhaser

================================================================================
Fuzzbox
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
Schroeder Reverb
================================================================================

This reverb implementation is based on Manfred Shroeder's model. A signal is
first passed through a parallel bank of feedback comb filters.  The output of
the comb filters are added and fed into a series of three all-pass filters.

Delay values of these all-pass filters are mutually prime to prevent any mutual
periodicity between their outputs.

> allPassSection :: AudSF Double Double
> allPassSection = proc s -> do
>       s1 <- filterAllPass (toS 347) (toS 347) 0 0.75 -< s
>       s2 <- filterAllPass (toS 113) (toS 113) 0 0.75 -< s1
>       s3 <- filterAllPass (toS 37) (toS 37) 0 0.75 -< s2
>       outA -< s3

> feedbackFilter :: Double -> Double -> Double -> AudSF Double Double
> feedbackFilter del a b = proc s -> do
>       rec d <- delayLine del -< s + (-a)*d
>       outA -< b*d

> schroederRev :: AudSF Double Double
> schroederRev = proc s -> do
>       c1 <- feedbackFilter (toS 1687) 0.805 1 -< s
>       c2 <- feedbackFilter (toS 1601) 0.825 1 -< s
>       c3 <- feedbackFilter (toS 2053) 0.845 1 -< s
>       c4 <- feedbackFilter (toS 2251) 0.873 1 -< s
>       f <- allPassSection -< (c1 + c2 + c3 + c4)
>       outA -< f

Test with a short note.

> tRev :: AudSF () Double
> tRev = proc () -> do
>       env <- envLineSeg [0,1,0,0] [0.05,0.25,5] -< ()
>       s <- clarinet 5 (absPitch (A, 5)) 3 [] -< ()
>       f <- schroederRev -< s*env
>       outA -< f

> testRev = outFile "shroeder.wav" 5 tRev

================================================================================
Composition
================================================================================

Welcome to R&D.

> flangerInstr :: Instr (Mono AudRate)
> flangerInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           s <- clarinet dur ap 3 [] -< ()
>           p <- flanger 0.006 0.020 0.5 0.7 -< s 
>           outA -< p / 10

> phaserInstr :: Instr (Mono AudRate)
> phaserInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           s <- clarinet dur ap 3 [] -< ()
>           p <- phaser 0.05 0.15 0.6 0.6 1 -< s 
>           outA -< p / 10

> chorusInstr :: Instr (Mono AudRate)
> chorusInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           s <- clarinet dur ap 3 [] -< ()
>           p <- chorus 0.6 1  -< s 
>           outA -< p / 15

> fuzzBoxInstr :: Instr (Mono AudRate)
> fuzzBoxInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           s <- clarinet dur ap 3 [] -< ()
>           p <- fuzzbox 0.1 -< s 
>           outA -< p / 10

> reverbInstr :: Instr (Mono AudRate)
> reverbInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           env <- envLineSeg [0,1,0,0] [0.05,0.25,5] -< ()
>           s <- clarinet 5 ap 3 [] -< ()
>           f <- schroederRev -< s*env
>           outA -< f / 10

-->           p <- fuzzbox 0.1 -< s

> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(myFlanger, flangerInstr), (myPhaser, phaserInstr),
>               (myChorus, chorusInstr), (myReverb, reverbInstr),
>               (myFuzzbox, fuzzBoxInstr)]

> myFlanger :: InstrumentName
> myFlanger = Custom "My Flanger"

> myPhaser :: InstrumentName
> myPhaser = Custom "My Phaser"

> myChorus :: InstrumentName
> myChorus = Custom "My Chorus"

> myReverb :: InstrumentName
> myReverb = Custom "My Reverb"

> myFuzzbox :: InstrumentName
> myFuzzbox = Custom "My Fuzzbox"

> pentatonicScale = [(absPitch (C,5)), (absPitch (D,5)), (absPitch (E,5)),
>                    (absPitch (G,5)), (absPitch (A,5)),
>                    (absPitch (C,4)), (absPitch (D,4)), (absPitch (E,4)),
>                    (absPitch (G,4)), (absPitch (A,4))]

> mel = [(absPitch (C,5)), (absPitch (D,5)), (absPitch (E,5))]

> myscifi1 :: Instr (Mono AudRate)
> myscifi1 dur ap vol [] =
>   let v = fromIntegral vol / 100 in proc () -> do
>       a1   <- noiseBLH 42 -< 5
>       a2   <- osc sinTab 0 -< (map(apToHz)pentatonicScale)!!(round ((a1^2) * 9))
>       outA -< a2 * v

> test3 = let a = (schroederRev <<< (myscifi1 10 (absPitch (C, 5)) 20 []))
>         in outFile "scifi.wav" 10 a

> kidcudi1 = [c 4, g 4, d 4, c 4]
> kidcudi2 = [af 3, ef 4, bf 3, af 3]

> p1a1 = line (fuse [wn] [kidcudi1!!0])
> p1b1 = line (fuse [wn] [kidcudi2!!0])
> p1a2 = line ([rest (wn)] ++ fuse [wn] [kidcudi1!!1])
> p1b2 = line ([rest (wn)] ++ fuse [wn] [kidcudi2!!1])
> p1a3 = line ([rest (wn*2)] ++ fuse [wn] [kidcudi1!!2])
> p1b3 = line ([rest (wn*2)] ++ fuse [wn] [kidcudi2!!2])
> p1a4 = line ([rest (wn*3)] ++ fuse [wn] [kidcudi1!!3])
> p1b4 = line ([rest (wn*3)] ++ fuse [wn] [kidcudi2!!3])

--> p1b = line (fuse [wn, wn, wn, wn] kidcudi2)

> p2a = line ([rest (wn*4)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p2b = line ([rest (wn*4)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi2)
> p3a = line ([rest (wn*8)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p3b = line ([rest (wn*8)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi2)
> p4a = line ([rest (wn*12)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p4b = line ([rest (wn*12)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi2)
> p5a = line ([rest (wn*16)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p5b = line ([rest (wn*16)] ++
>            fuse [wn+qn, qn*3, wn, wn] kidcudi2)

> (d1a1, sf1a1) = renderSF (instrument myReverb p1a1) myInstrMap
> (d1b1, sf1b1) = renderSF (instrument myReverb p1b1) myInstrMap
> (d1a2, sf1a2) = renderSF (instrument myReverb p1a2) myInstrMap
> (d1b2, sf1b2) = renderSF (instrument myReverb p1b2) myInstrMap
> (d1a3, sf1a3) = renderSF (instrument myReverb p1a3) myInstrMap
> (d1b3, sf1b3) = renderSF (instrument myReverb p1b3) myInstrMap
> (d1a4, sf1a4) = renderSF (instrument myReverb p1a4) myInstrMap
> (d1b4, sf1b4) = renderSF (instrument myReverb p1b4) myInstrMap

> testRMel = outFile "rMel.wav" 5 sf1a2

--> (d1a, sf1a) = renderSF (instrument myReverb p1a) myInstrMap
--> (d1b, sf1b) = renderSF (instrument myReverb p1b) myInstrMap

> (d2a, sf2a) = renderSF (instrument myFlanger p2a) myInstrMap
> (d2b, sf2b) = renderSF (instrument myFlanger p2b) myInstrMap

> (d3a, sf3a) = renderSF (instrument myPhaser p3a) myInstrMap
> (d3b, sf3b) = renderSF (instrument myPhaser p3b) myInstrMap

> (d4a, sf4a) = renderSF (instrument myFuzzbox p4a) myInstrMap
> (d4b, sf4b) = renderSF (instrument myFuzzbox p4b) myInstrMap

> (d5a, sf5a) = renderSF (instrument myChorus p5a) myInstrMap
> (d5b, sf5b) = renderSF (instrument myChorus p5b) myInstrMap

> comp ::  AudSF () Double
> comp = proc () -> do
>       s1a1 <- sf1a1 -< ()
>       s1b1 <- sf1b1 -< ()
>       s1a2 <- sf1a2 -< ()
>       s1b2 <- sf1b2 -< ()
>       s1a3 <- sf1a3 -< ()
>       s1b3 <- sf1b3 -< ()
>       s1a4 <- sf1a4 -< ()
>       s1b4 <- sf1b4 -< ()

>       s2a <- sf2a -< ()
>       s2b <- sf2b -< ()
>       s3a <- sf3a -< ()
>       s3b <- sf3b -< ()
>       s4a <- sf4a -< ()
>       s4b <- sf4b -< ()
>       s5a <- sf5a -< ()
>       s5b <- sf5b -< ()
>       outA -< s1a1 + s1b1 + s1a2 + s1b2 + s1a3 + s1b3 + s1a4 + s1b4 +
>               s2a + s2b + s3a + s3b + s4a + s4b + s5a + s5b

-->       a <- myscifi1 10 (absPitch (C, 5)) 20 [] -< ()

> testComp = outFile "comp.wav" 8 comp