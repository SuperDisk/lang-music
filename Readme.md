# #lang music

is a project that Michael Gunantra and I did for IU's P424 Advanced Functional Programming class. It's a music notation language inspired by MML, but instead of manipulating and concatenating strings, it's a structured language with full support for manipulating notes and bars.

It was done in Racket and outputs MIDI data. The playback routine is still a little buggy but it's a fun prototype to play with nonetheless.

# How do I use this?

The idea behind composing in **#lang music** is that a song is a stream of notes, and you are given a few basic "primitive" streams of notes with which you can build bigger compositions. The primitive "notes," which are really just songs with a length of one note, are **a** through **g**, followed optionally by a sharp sign, duration, and dot.

A sharp brings the note up by one semitone, a duration specifies the length of the note (half, quarter, etc), and the dot increases the duration by half of its value, like in regular old music notation.

Here's the "docs." I'll probably eventually make some proper Scribble docs but here's a list of functions:

    (defun <name> (<arg> ...) <seq> ...)
    
Defines a musical function. Return a list of sequences. Any call to a function defined with `defun` will be considered a sequence.
    
    (seq <seq> ...)
    
Sequences notes. Most blocks do this by default, but this can be useful inside a `together` to allow for serial note playback again.
    
    (drumseq <seq> ...)
    
Same as a regular `seq` except all notes played are on the drum track.

    (loop <num> <seq> ...)
    
Loops the sequences `num` number of times. If `num` is zero, then loop the sequences infinitely.
    
    (octave+ <num> <seq> ...)
    
Brings up the provided sequences by `num` octaves. `num` can be negative.
    
    (semitone+ <num> <seq> ...)
    
Brings up the provided sequnces by `num` semitones. `num` can be negative.
    
    (with-voice <voice> <seq> ...)
    
Sets the voices of the provided sequences. See `voices.rkt` for what voices you can use.
    
    (together <seq> ...)
    
Plays the provided sequences simultaneously. Plays until the shortest provided sequence runs out of notes, upon which the other sequences will be cut short.
    
    (play <seq> ...)
    
Converts sequences into MIDI control data.
    
    (dump-to-midi <filename> <midi-control-data>)
    
Converts MIDI control data into a MIDI file.

Some other niceties:

`>` and `<` aren't the greater than and less than operators anymore (use `lt?` and `gt?` instead), they function as "notes" that bring the current octave up and down respectively. You can also use `r` for resting in the style of a regular note, and `o?` where `?` is in the range of 0-5 to set the octave explicitly.

# Example
Here's an example tune programmed by Michael, the intro to _Sweet Child 'O Mine_.

    #lang music
    
    (defseq lead1
      > r8 d8 a8 < g8 > g8 a8 f#8 a8 <)

    (defseq lead2
      d8 > d8 a8 < g8 > g8 a8 f#8 a8 <)

    (defseq lead3
      e8 > d8 a8 < g8 > g8 a8 f#8 a8 <)

    (defseq lead4
      g8 > d8 a8 < g8 > g8 a8 f#8 a8 <)

    (defseq lead5
      > e8 a8 d8 a8 e8 a8 f#8 a8
      g8 a8 f#8 a8 e8 a8 d4 <)


    (defseq bass1
      d
      d
      e
      e
      g
      g
      d
      d2. r8 < <  r32 d32 f#32 > a32 >)

    (defseq bass2a
      < < d4. > f#4. f#8 g8 
      > a4 b8 a8 < g8 f#8 d8 < r32 c32 e32 g32
      c2 > g2
      > d8 c4 d4. c8 < < r32 g32 > b32 d32
      < g2. > r8 r32 g32 > b32 d32
      g)

    (defseq bass2b
      < < f#2. r4
      r
      d2 r2
      r
      > b2. r4
      > b
      )

    (defseq bass2c
      < a2. r4
      r
      g2 r2
      r
      > d2. r4
      > d  <
      )

    (defseq bass3a
      < < d
      d > >)

    (defseq bass3b
      < a
      a >)

    (defseq bass3c
      < d
      d >)

    (dump-to-midi
     "sweet.mid"
     (play
      (together (with-voice electric-guitar-clean (seq (loop 8 lead1)
                     (loop 2 lead2)
                     (loop 2 lead3)
                     (loop 2 lead4)
                     (loop 2 lead2)))
                (with-voice electric-bass-finger bass1)
                (loop 0 (with-voice side-stick (drumseq lead1)))
                )

      (together (seq (loop 2 lead2)
                     (loop 2 lead3)
                     (loop 2 lead4)
                     lead5)
                (seq bass2a bass3a)
                (seq bass2b bass3b)
                (seq bass2c bass3c)
      )))
