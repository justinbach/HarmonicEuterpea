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


The following helper functions provide a shorthand means of invoking a given player
in the performance of a piece of music.

Use the "clean" player to perform a melody.

> playWithPlayer     :: String -> Music Pitch -> IO ()
> playWithPlayer p m = playA myPMap defCon $ toMusic1 $ Modify (Player p) $ m

> playClean :: Music Pitch -> IO ()
> playClean m = playWithPlayer "CleanPlayer" m

> playRich :: Music Pitch -> IO ()
> playRich m = playWithPlayer "RichPlayer" m

> playTritone :: Music Pitch -> IO ()
> playTritone m = playWithPlayer "TritonePlayer" m

> playReharm :: Music Pitch -> IO ()
> playReharm m = playWithPlayer "ReharmPlayer" m

> playCombo :: Music Pitch -> IO ()
> playCombo m = playWithPlayer "ComboPlayer" m
