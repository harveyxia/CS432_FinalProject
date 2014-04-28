********************************************************************************
|                                                                              |
|                             CS432 FINAL PROJECT                              |
|                               April 28, 2014                                 |
|                                                                              |
|                         by Jason Kim and Harvey Xia                          |
|                                                                              |
********************************************************************************
================================================================================
PROJECT ABSTRACT
================================================================================

We sought to implement various sound effects using signal
functions. Our approach placed a greater emphasis on theoretical and technical
fidelity to the effects as they were conceived and implemented in hardware as
well as on aesthetics. We've included code at the end of the file to showcase
each of our effects as rendered in a wav file.

Run "effectsShowcase" to render the wav file.

================================================================================
THOUGHTS AND ANALYSIS
================================================================================

During our work on this project, we arrived at the realization that some effects
were substantially more difficult to implement than others, often due to either
an absence of online resources or a genuine difference in the complexity of
the effects we were attempting to implement.

However, another substantial difficulty we faced was the difficulty of
generating certain sound effects in a digital medium. Often, canonical sound
effects originate from the physical characteristics of instruments and audio
devices. In many of these cases, the effects were discovered by accident. For
instance, the harsh fuzzbox distortion effect we've implemented in Haskell below
was originally the result of overdriving a tube amplifier, a now bygone piece of
audio equipment. But in several instances, the relative ease of physically
realizing a sound effect came into sharp contrast with the difficulty of
realizing those effects in code (this was certainly the case for the reverb,
phaser, and flanger effects).

A more fundamental problem in implementing sound effects was determining what
sort of sound we wanted to produce. Sometimes, after implementing an effect, we
would be disappointed to discover that the effect simply sounded bad. While it
wasn't hard to find examples of each of these effects online, it was a different
story when it came to determining what an effect *ought* to sound like. Many of
the examples we found online were either demos of electric guitar effects units
or "impure" exaggerations of the original effects, of which we struggled to find
even two that agreed.

In the end, it took a fair bit of research and head-scratching while staring at
block diagrams of signal processing circuits to acheive the desired effects, but
we hope that the effort shows in the quality of these effects.

================================================================================
SOURCES
================================================================================
- https://ccrma.stanford.edu/~jos/pasp/Flanging.html
- http://en.wikipedia.org/wiki/Phaser_(effect)
- https://ccrma.stanford.edu/~jos/pasp/Allpass_Two_Combs.html
- https://ccrma.stanford.edu/~jos/pasp/Phasing_First_Order_Allpass_Filters.html
- https://ccrma.stanford.edu/~jos/pasp/Schroeder_Reverberators.html
- http://dspwiki.com/index.php?title=Reverberation#Manfred_Schroeder_Reverberation
- https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/processing2.html
- http://en.wikipedia.org/wiki/Distortion_(music)
- http://en.wikipedia.org/wiki/Wah-wah_(music)#Technique
- http://www.cs.sfu.ca/~tamaras/delayEffects/Implementation_Chorus.html

================================================================================
Import Libraries and Utility Code
================================================================================

> {-# LANGUAGE Arrows #-}
> module Final where
> import Euterpea
> import Control.Arrow ((>>>), (<<<), arr)
> import Data.Complex


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

> fuse       :: [Dur] -> [Dur -> Music a] -> [Music a]
> fuse (d:ds) (n:ns) = (n d) : fuse ds ns
> fuse [] []         = []

toS is a function that converts a delay time in terms of table size to its
corresponding delay in terms of seconds. In other words, toS outputs the seconds
that it would take to sample through a table of size s at a rate of 44.1 kHz.

> toS :: Double -> Double
> toS s = (s / 44100)

================================================================================
Flanger
================================================================================

An effect that uses delay to create an interesting sound.

dmin = minimum delay time in seconds
dmax = maximum delay time in seconds
freq = frequency of the low frequency modulator (for delay time)
depth = amplitude multiplier for flanged signal

The flanger output sound is the sum of the original signal and a copy of that
signal delayed by a modulating duration at a speed given by freq. We found that
the best sound occured with a delay that modulated between 6 to 20 milliseconds
at a frequency between 0.3 and 1.

This flanger also utilizes feedback, as shown in the line starting with "rec g".
This feedback serves to intensify the flanger effect.

If depth is set to a negative value, the flanger is in inverted mode.

> flanger :: Double -> Double -> Double -> Double -> AudSF Double Double
> flanger dmin dmax freq depth =
>       let middle = (dmin + dmax)/2
>           mod = dmax - middle
>       in proc s -> do
>           sin <- oscI sinTab 0 -< freq
>           rec g <- delayLine1 1 -< (s + 0.4*g, middle + (mod * sin))
>           outA -< depth*g + s

Test Flanger

> tFlanger :: AudSF () Double
> tFlanger = proc () -> do
>       s <- clarinet 5 (absPitch (A, 3)) 3 [] -< ()
>       f <- flanger 0.006 0.020 1 0.3 -< s
>       outA -< f/5

> testFlanger = outFile "flanger.wav" 5 tFlanger

================================================================================
Chorus
================================================================================

An effect that simulates the sound of several voices or instruments at roughly
the same timbre and pitch, as is the case with a choir or an orchestra.

freq = frequency of low frequency modulator
depth = amplitude coefficient for chorus effect

This effect is implemented with 4 delay lines, each delaying the original signal
by a time-varying amount (between 20 and 50 ms), according to sin waves that are
each offset by a different phase. These 4 delay lines correspond to 4 different
voices, that sound slightly different but still in unison.

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
================================================================================

An effect that uses phase shifting to create an interesting undulating sound.

This effect was realized by an all-pass filter that shifts the phase of a
signal by a varying amount determined by the frequency of a sinusoidal
modulator signal.

dmin = minimum delay time in seconds
dmax = maximum delay time in seconds
freq = frequency of the low frequency modulator (for delay time)
g = feedforward and feedbackward coefficient

The all-pass filter filters all frequencies at a gain of 1, not changing the
amplitude. The version we implemented is a two-comb all-pass filter: a feedback
comb filter paired with a feedforward comb filter, both with equal delays.
Our implementation is based on the diagram found at:
https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/ap.gif

> filterAllPass :: Double -> Double -> Double -> Double -> AudSF Double Double
> filterAllPass dmin dmax freq g =
>       let middle = (dmin + dmax)/2
>           mod = dmax - middle
>       in proc s -> do
>           sin <- osc sinTab 0.25 -< freq
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

Test the phaser:

> tPhaser :: AudSF () Double
> tPhaser = proc () -> do
>       s <- clarinet 5 (absPitch (A, 5)) 3 [] -< ()
>       f <- phaser 0.05 0.15 0.6 0.6 1 -< s
>       outA -< f / 5

> testPhaser = outFile "phaser.wav" 10 tPhaser

================================================================================
Fuzzbox
================================================================================

An effect that clips off the peaks and troughs of a signal to produce a
distorted, "dirty" sound, as with an electric guitar.

This effect is simple to realize, but this comes at the cost of robustness.
Since it merely limits the amplitude of the input signal but cannot actively
detect the amplitude of a signal at a given time, a very soft signal and/or a
low depth value may fail to produce the distortion effect.

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

An effect that emulates the acoustic qualities endowed to a sound when it is
produced in a particular physical space.

This reverb implementation is based on Manfred Shroeder's model. A signal is
first passed through a parallel bank of feedback comb filters.  The output of
the comb filters are added and fed into a series of three all-pass filters.

Delay values of these all-pass filters are mutually prime to prevent any mutual
periodicity between their outputs.

In more detail: the feedback comb filters simulate a pair of walls, and their
delay time represents how long it takes a wave to travel from one wall to the 
opposite wall and back. The all-pass filters determine the intensity of the
echo.

> allPassSection :: AudSF Double Double
> allPassSection = proc s -> do
>       s1 <- filterAllPass (toS 1019) (toS 1019) 0 0.75 -< s
>       s2 <- filterAllPass (toS 347) (toS 347) 0 0.75 -< s1
>       s3 <- filterAllPass (toS 113) (toS 113) 0 0.75 -< s2
>       outA -< s3

> feedbackFilter :: Double -> Double -> Double -> AudSF Double Double
> feedbackFilter del a b = proc s -> do
>       rec d <- delayLine del -< s + (-a)*d
>       outA -< b*d

> schroederRev :: AudSF Double Double
> schroederRev = proc s -> do
>       c1 <- feedbackFilter (toS 2452) 0.805 1 -< s
>       c2 <- feedbackFilter (toS 2743) 0.825 1 -< s
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
Wahwah
================================================================================

An effect that gives a sound an oscillating "wah wah" sound.

This effect takes a signal and oscillates the amplitude of its tremble
frequencies, producing a sound similar to someone making a "wah wah" sound.
To realize this effect, our implementation splits the input signal and runs each
through a bandpass filter--one through a lowpass and other other through a high-
pass. The signal run through the highpass filter is then multiplied by an
oscillating value, varying its amplitude. The two signals are then recombined,
producing a single signal whose higher frequencies sinusoidally fade in and out.

freq = the frequency of the treble amplitude oscillation
dep  = the depth of the oscillation

> wahwah :: Double -> Double -> AudSF Double Double
> wahwah freq dep =
>   proc s -> do
>       sin  <- osc sinTab 0   -< freq
>       treb <- filterLowPass  -< (s, 1900)
>       bass <- filterHighPass -< (s, 1800)
>       outA -< (bass + treb * ((dep * sin) + 0.5 * dep)) / 2

> wahSig :: AudSF () Double
> wahSig =
>   proc () -> do
>       s1 <- osc sinTab 0 -< 440
>       s2 <- osc sinTab 0 -< 560
>       s3 <- osc sinTab 0 -< 700
>       s4 <- osc sinTab 0 -< 880
>       s5 <- osc sinTab 0 -< 1760
>       outA -< (s1 + s2 + s3 + s4 + s5) / 5

> testWahwah = outFile "wah.wav" 5 (wahSig >>> wahwah 4 1)

================================================================================
Composition
================================================================================

Below, we've provided code that showcases our sound effects. Most of it is
overhead for the comp function, which generates the final .wav file. The
wav file takes some time to render (it's about 8.5 mb). It demonstrates each
of the effects in the following succession:

1. Schroeder reverb
2. Flanger
3. Phaser
4. Fuzzbox
5. Chorus
6. Wahwah

> flangerInstr :: Instr (Mono AudRate)
> flangerInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           s <- clarinet dur ap 3 [] -< ()
>           p <- flanger 0.006 0.020 1 0.6 -< s 
>           outA -< p / 13

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
>           outA -< p / 20

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
>           outA -< f*3

> wahwahInstr :: Instr (Mono AudRate)
> wahwahInstr dur ap vol [] =
>       let f     = apToHz ap
>           v     = fromIntegral vol
>           d     = fromRational dur
>       in proc () -> do
>           s <- clarinet dur ap 3 [] -< ()
>           p <- wahwah 4 1 -< s 
>           outA -< p / 10

> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(myFlanger, flangerInstr), (myPhaser, phaserInstr),
>               (myChorus, chorusInstr), (myReverb, reverbInstr),
>               (myFuzzbox, fuzzBoxInstr), (myWahwah, wahwahInstr)]

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

> myWahwah :: InstrumentName
> myWahwah = Custom "My Wahwah"

> kidcudi1 = [c 4, g 4, d 4, c 4]
> kidcudi2 = [af 3, ef 4, bf 3, af 3]

> p1a1 = line (fuse [wn] [kidcudi1!!0])
> p1b1 = line (fuse [wn] [kidcudi2!!0])
> p1a2 = line ([rest (wn)]   ++ fuse [wn] [kidcudi1!!1])
> p1b2 = line ([rest (wn)]   ++ fuse [wn] [kidcudi2!!1])
> p1a3 = line ([rest (wn*2)] ++ fuse [wn] [kidcudi1!!2])
> p1b3 = line ([rest (wn*2)] ++ fuse [wn] [kidcudi2!!2])
> p1a4 = line ([rest (wn*3)] ++ fuse [wn] [kidcudi1!!3])
> p1b4 = line ([rest (wn*3)] ++ fuse [wn] [kidcudi2!!3])

> p2a = line ([rest (wn*4)]  ++ fuse [wn*2, wn*2] [c 4, g 4])
> p2b = line ([rest (wn*4)]  ++ fuse [wn*2, wn*2] [af 3, ef 3])
> p3a = line ([rest (wn*8)]  ++ fuse [wn*2, wn*2] [d 4, c 4])
> p3b = line ([rest (wn*8)]  ++ fuse [wn*2, wn*2] [bf 3, af 3])
> p4a = line ([rest (wn*12)] ++ fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p4b = line ([rest (wn*12)] ++ fuse [wn+qn, qn*3, wn, wn] kidcudi2)
> p5a = line ([rest (wn*16)] ++ fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p5b = line ([rest (wn*16)] ++ fuse [wn+qn, qn*3, wn, wn] kidcudi2)
> p6a = line ([rest (wn*20)] ++ fuse [wn+qn, qn*3, wn, wn] kidcudi1)
> p6b = line ([rest (wn*20)] ++ fuse [wn+qn, qn*3, wn, wn] kidcudi2)

> (d1a1, sf1a1) = renderSF (instrument myReverb p1a1) myInstrMap
> (d1b1, sf1b1) = renderSF (instrument myReverb p1b1) myInstrMap
> (d1a2, sf1a2) = renderSF (instrument myReverb p1a2) myInstrMap
> (d1b2, sf1b2) = renderSF (instrument myReverb p1b2) myInstrMap
> (d1a3, sf1a3) = renderSF (instrument myReverb p1a3) myInstrMap
> (d1b3, sf1b3) = renderSF (instrument myReverb p1b3) myInstrMap
> (d1a4, sf1a4) = renderSF (instrument myReverb p1a4) myInstrMap
> (d1b4, sf1b4) = renderSF (instrument myReverb p1b4) myInstrMap
> (d2a, sf2a) = renderSF (instrument myFlanger p2a) myInstrMap
> (d2b, sf2b) = renderSF (instrument myFlanger p2b) myInstrMap
> (d3a, sf3a) = renderSF (instrument myPhaser p3a) myInstrMap
> (d3b, sf3b) = renderSF (instrument myPhaser p3b) myInstrMap
> (d4a, sf4a) = renderSF (instrument myFuzzbox p4a) myInstrMap
> (d4b, sf4b) = renderSF (instrument myFuzzbox p4b) myInstrMap
> (d5a, sf5a) = renderSF (instrument myChorus p5a) myInstrMap
> (d5b, sf5b) = renderSF (instrument myChorus p5b) myInstrMap
> (d6a, sf6a) = renderSF (instrument myWahwah p6a) myInstrMap
> (d6b, sf6b) = renderSF (instrument myWahwah p6b) myInstrMap

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
>       s6a <- sf6a -< ()
>       s6b <- sf6b -< ()
>       outA -< s1a1/7 + s1b1/7 + s1a2 + s1b2 + s1a3 + s1b3 + s1a4/7 + s1b4/7 +
>               s2a + s2b + s3a + s3b + s4a + s4b + s5a + s5b + s6a + s6b

Run this code in order to generate our showcase .wav file. Note: rendering took
us up to five minutes.

> effectsShowcase = outFile "EffectsShowcase.wav" 48 comp