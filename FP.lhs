Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module FP where


For my final project, I added explicit harmonic context to Euterpea in a form
approximating that of a lead sheet. My project consists of the following files,
each of which is accompanied by an explanation. (Each file itself also contains
commentary and explanation as necessary). A general discussion of the project
follows the description of each file.


> import EuterpeaMods

EuterpeaMods contains a "shadow" version of the Euterpea library, as well as the
modifications that I had to make to add support for explicit harmonic context.
The changes are principally the addition of a new PhraseAttribute constructor
called Chord, which takes a PitchClass and a ChordType (a new data type) as
arguments, and the threading of a state monad through the performance-related
functions to properly handle random number generation without using
unsafePerformIO.


> import ChordFuns

ChordFuns is a collection of functions used during the creation of harmonic textures based on harmonic annotations.


> import Players

Players contains four new Euterpea Players that are able to interpret harmonic
annotations. They range in harmonic approach from the conservative to the
outrageous, and a detailed description of each is available prior to its
definition. It also defines a number of helper functions that allow shorthand
invocation of a player on a given piece of music (e.g. "playReharm").

> import LeadSheets

Leadsheets contains four complete of harmonically-annotated melodies that can be
used to test the new players. The selections are all well-known jazz standards,
and their transcription is fairly typical (modulo a musical "hack" described in
the file header).



