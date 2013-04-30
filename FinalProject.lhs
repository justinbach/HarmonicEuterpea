Name:   Justin Bachorik
NetID:  jpb55
Class:  CPSC-531
Date:   4/15/13

Final Project

> module FinalProject where

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

ChordFuns is a collection of functions used during the creation of harmonic
textures based on harmonic annotations. Almost every function is annotated with
a description of its purpose, so the file itself is best consulted for specific
details.


> import Players

Players contains four new Euterpea Players that are able to interpret harmonic
annotations. They range in harmonic approach from the conservative to the
outrageous, and a detailed description of each is available prior to its
definition. It also defines a number of helper functions that allow shorthand
invocation of a player on a given piece of music (e.g. "playReharm m").


> import LeadSheets

Leadsheets contains four complete harmonically-annotated melodies that can be
used to test the new players. The selections are all well-known jazz standards,
and their transcription is fairly typical (modulo a musical "hack" described in
the file header).


GENERAL DISCUSSION

I was primarily motivated to add explicit harmonic to Euterpea because I was
fascinated by Euterpea's model of dividing denotation (Music values) and
performance (Performance values). As a performing musician, where exactly the
line between denotation and interpretation falls depends upon the genre of
music being played; generally speaking, classical scores make more explicit
demands regarding the notes played by a performer than a jazz lead sheet, which
might require that the melody be played along with some version of the denoted
harmony. It could be argued that some fully-improvised genres performances have
no denotational aspect whatsoever, and that the music consists solely of the
interpretation (e.g. some of Keith Jarrett's solo concerts).

After building in support for harmonic context and creating players that can
interpret chord annotations at the performance level, I think I can fairly say
that my project constitutes a reprehensible abuse of the Euterpean model of
performances. Constructing all chordal voicings in the Performance domain, as I
have, means that all chords must be built solely on the basis of the current
context, the harmonic annotation, and whatever Events (melody notes) appear
within the current phrase attribute handler. There are other limitations imposed
as well. For example, if in a portion of a lead sheet there appears a change in
harmony but no accompanying change in melody, no events will be generated, so no
harmony can be realized. I worked around this by using a few musical "hacks" in
my lead sheets, adding additional notes to the melody to trigger chord changes
when necessary. Finally, the lack of greater contextual information available
when interpreting a chord is limiting in terms of the creativity that can be
used in the voicings; there are hundreds of ways of reharmonizing the
ii7-V7-IMaj7 progression found in almost every tune in the great American
songbook, but in my scheme there's no way for a PhraseAttribute handler to know
that the ii7 it's currently handling is the first chord in that sequence.

Despite these shortcomings, I feel that this excursion was largely successful.
The Players (and the functions they rely on) are the heart and soul of this
project, and I feel that the RichPlayer, in particular, does a competent job
of interpreting harmonic annotation. The ReharmPlayer is quite a bit of fun,
as well; some of the interpretations it produces are strikingly unusual. And the
ComboPlayer nicely ties the new players together, creating performances that
vary from the conventional to the unexpected, sometimes dramatically.

I also learned quite a bit from this project, particularly about the
difficulty of encoding rules about harmony in a systematic manner (a lot of
the code that the RichPlayer uses for adding color tones is pretty
ad-hoc and unsatisfying). I also learned quite a bit about state monads, as I
had to thread them through the Performance functions in order to properly
handle random number generation.

Here are a few examples of how to use my module:

*FinalProject> playRich bodyAndSoul

*FinalProject> playReharm someWhereOverTheRainbow

*FinalProject> playCombo whenIFallInLove



