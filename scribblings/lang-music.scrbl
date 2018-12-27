#lang scribble/base

@title{Music, A Music Notation Language in Racket}
@author[(author+email @elem Nick
                      "npfaro@indiana.edu"
                      #:obfuscate? #t)]

@table-of-contents[]

@section{#:tag 'started'}{Getting Started}

The idea behind composing in #lang music is that a song is a stream of notes, and you are given a few basic "primitive" streams of notes with which you can build bigger compositions. The primitive "notes," which are really just songs with a length of one note, are a through g, followed optionally by a sharp sign, duration, and dot.
A sharp brings the note up by one semitone, a duration specifies the length of the note (half, quarter, etc), and the dot increases the duration by half of its value, like in regular old music notation.

@defform[(defun <name> (<arg> ...) <seq> ...)]

Defines a musical function. Return a list of sequences. Any call to a function defined with defun will be considered a sequence.

@defform[(seq <seq> ...)]

Sequences notes. Most blocks do this by default, but this can be useful inside a together to allow for serial note playback again.

@defform[(drumseq <seq> ...)]

Same as a regular seq except all notes played are on the drum track.

@defform[(loop <num> <seq> ...)]

Loops the sequences num number of times. If num is zero, then loop the sequences infinitely.

@defform[(octave+ <num> <seq> ...)]

Brings up the provided sequences by num octaves. num can be negative.

@defform[(semitone+ <num> <seq> ...)]

Brings up the provided sequnces by num semitones. num can be negative.

@defform[(with-voice <voice> <seq> ...)]

Sets the voices of the provided sequences. See @secref{voices} for what voices you can use.

@defform[(together <seq> ...)]

Plays the provided sequences simultaneously. Plays until the shortest provided sequence runs out of notes, upon which the other sequences will be cut short.

@defform[(play <seq> ...)]

Converts sequences into MIDI control data.

@defform[(dump-to-midi <filename> <midi-control-data>)]

Converts MIDI control data into a MIDI file.

@section{#:tag 'niceties'}{Some other niceties}

> and < aren't the greater than and less than operators anymore (use lt? and gt? instead), they function as "notes" that bring the current octave up and down respectively. You can also use r for resting in the style of a regular note, and o? where ? is in the range of 0-5 to set the octave explicitly.

@section{#:tag 'voices'}{Voices}

@tabular[#:sep @hspace[1]
         (list (list @bold{Voice} @bold{Id})
               ;; Regular MIDI Voices. These can be used on any channel but 10.
               (list acoustic-grand-piano 1)
               (list bright-acoustic-piano 2)
               (list electric-grand-piano 3)
               (list honky-tonk-piano 4)
               (list electric-piano-1 5)
               (list electric-piano-2 6)
               (list harpsichord 7)
               (list clavi 8)
               (list celesta 9)
               (list glockenspiel 10)
               (list music-box 11)
               (list vibraphone 12)
               (list marimba 13)
               (list xylophone 14)
               (list tubular-bells 15)
               (list dulcimer 16)
               (list drawbar-organ 17)
               (list percussive-organ 18)
               (list rock-organ 19)
               (list church-organ 20)
               (list reed-organ 21)
               (list accordion 22)
               (list harmonica 23)
               (list tango-accordion 24)
               (list acoustic-guitar-nylon 25)
               (list acoustic-guitar-steel 26)
               (list electric-guitar-jazz 27)
               (list electric-guitar-clean 28)
               (list electric-guitar-muted 29)
               (list overdriven-guitar 30)
               (list distortion-guitar 31)
               (list guitar-harmonics 32)
               (list acoustic-bass 33)
               (list electric-bass-finger 34)
               (list electric-bass-pick 35)
               (list fretless-bass 36)
               (list slap-bass-1 37)
               (list slap-bass-2 38)
               (list synth-bass-1 39)
               (list synth-bass-2 40)
               (list violin 41)
               (list viola 42)
               (list cello 43)
               (list contrabass 44)
               (list tremolo-strings 45)
               (list pizzicato-strings 46)
               (list orchestral-harp 47)
               (list timpani 48)
               (list string-ensemble-1 49)
               (list string-ensemble-2 50)
               (list synthstrings-1 51)
               (list synthstrings-2 52)
               (list choir-aahs 53)
               (list voice-oohs 54)
               (list synth-voice 55)
               (list orchestra-hit 56)
               (list trumpet 57)
               (list trombone 58)
               (list tuba 59)
               (list muted-trumpet 60)
               (list french-horn 61)
               (list brass-section 62)
               (list synthbrass-1 63)
               (list synthbrass-2 64)
               (list soprano-sax 65)
               (list alto-sax 66)
               (list tenor-sax 67)
               (list baritone-sax 68)
               (list oboe 69)
               (list english-horn 70)
               (list bassoon 71)
               (list clarinet 72)
               (list piccolo 73)
               (list flute 74)
               (list recorder 75)
               (list pan-flute 76)
               (list blown-bottle 77)
               (list shakuhachi 78)
               (list whistle 79)
               (list ocarina 80)
               (list lead-1-square 81)
               (list lead-2-sawtooth 82)
               (list lead-3-calliope 83)
               (list lead-4-chiff 84)
               (list lead-5-charang 85)
               (list lead-6-voice 86)
               (list lead-7-fifths 87)
               (list lead-8-bass-+-lead 88)
               (list pad-1-new-age 89)
               (list pad-2-warm 90)
               (list pad-3-polysynth 91)
               (list pad-4-choir 92)
               (list pad-5-bowed 93)
               (list pad-6-metallic 94)
               (list pad-7-halo 95)
               (list pad-8-sweep 96)
               (list fx-1-rain 97)
               (list fx-2-soundtrack 98)
               (list fx-3-crystal 99)
               (list fx-4-atmosphere 100)
               (list fx-5-brightness 101)
               (list fx-6-goblins 102)
               (list fx-7-echoes 103)
               (list fx-8-sci-fi 104)
               (list sitar 105)
               (list banjo 106)
               (list shamisen 107)
               (list koto 108)
               (list kalimba 109)
               (list bag-pipe 110)
               (list fiddle 111)
               (list shanai 112)
               (list tinkle-bell 113)
               (list agogo 114)
               (list steel-drums 115)
               (list woodblock 116)
               (list taiko-drum 117)
               (list melodic-tom 118)
               (list synth-drum 119)
               (list reverse-cymbal 120)
               (list guitar-fret-noise 121)
               (list breath-noise 122)
               (list seashore 123)
               (list bird-tweet 124)
               (list telephone-ring 125)
               (list helicopter 126)
               (list applause 127)
               (list gunshot 128)

               ;; MIDI Drums. Only can be used on channel 10.
               (list acoustic-bass-drum 35)
               (list bass-drum-1 36)
               (list side-stick 37)
               (list acoustic-snare 38)
               (list hand-clap 39)
               (list electric-snare 40)
               (list low-floor-tom 41)
               (list closed-hi-hat 42)
               (list high-floor-tom 43)
               (list pedal-hi-hat 44)
               (list low-tom 45)
               (list open-hi-hat 46)
               (list low-mid-tom 47)
               (list hi-mid-tom 48)
               (list crash-cymbal-1 49)
               (list high-tom 50)
               (list ride-cymbal-1 51)
               (list chinese-cymbal 52)
               (list ride-bell 53)
               (list tambourine 54)
               (list splash-cymbal 55)
               (list cowbell 56)
               (list crash-cymbal-2 57)
               (list vibraslap 58)
               (list ride-cymbal-2 59)
               (list hi-bongo 60)
               (list low-bongo 61)
               (list mute-hi-conga 62)
               (list open-hi-conga 63)
               (list low-conga 64)
               (list high-timbale 65)
               (list low-timbale 66)
               (list high-agogo 67)
               (list low-agogo 68)
               (list cabasa 69)
               (list maracas 70)
               (list short-whistle 71)
               (list long-whistle 72)
               (list short-guiro 73)
               (list long-guiro 74)
               (list claves 75)
               (list hi-wood-block 76)
               (list low-wood-block 77)
               (list mute-cuica 78)
               (list open-cuica 79)
               (list mute-triangle 80)
               (list open-triangle 81))]