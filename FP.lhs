Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module FP where
> import LeadSheets
> import Players
> import Euterpea
> import Data.List
> import Data.Ratio
> import Data.Maybe

This file serves to demonstrate some of the uses of the new Chord PhraseAttribute I've added to Music.hs. In particular, it creates several chord-tagged melodies, equivalent to a lead sheet, and constructs a few different players that perform them, interpreting the underlying harmony in different ways.


Example of usage:

*FP> perform myPMap defCon $ toMusic1 $ Modify (Player "DiatonicPlayer") $ somewhere
