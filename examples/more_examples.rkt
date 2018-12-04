#lang racket

(require "../langmusic.rkt")

(defseq game
  > e
  a
  c4 d4 e2
  a2 c4 d4
  b
  d
  < g > 
  c4 b4 d2
  < g >
  c4 b4 a2
  r
  r
  r
  <)

(defseq sample
  c8 c8 c8 c8 c8 c8 c8 c8)


  
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

;;drumseq
#;(dump-to-midi
 "sweet1.mid"
 (play
  (with-voice side-stick (drumseq lead1))))

;;gameOfThrones
(dump-to-midi
 "ifEverybodyDiesInGameofThrones.mid"
 (play
  game))

;;dump-to-midi
(dump-to-midi
 "sample.mid"
 (play
  sample))

;;octave+
(dump-to-midi
 "sampleOctave+.mid"
 (play
  (octave+  1 sample)))

;;octave-
(dump-to-midi
 "sampleOctave-.mid"
 (play
  (octave+ -1 sample)))

;;with-voice
(dump-to-midi
 "guitar.mid"
 (play
  (with-voice acoustic-guitar-nylon sample)))

;;loop
(dump-to-midi
 "loop.mid"
 (play
  (loop 4 sample)))

;;together
(dump-to-midi
 "together.mid"
 (play
  (together sample
            (octave+ 1 sample))))

;;>
(dump-to-midi
 "up.mid"
 (play
  (seq sample > sample < sample)))


;;<
(dump-to-midi
 "up.mid"
 (play
  (seq sample < sample > sample)))



#;(provide defun seq drumseq defseq loop octave+ with-voice octave-
         together play > < dump-to-midi
         (all-from-out "voices.rkt"))

#;(define (octave- num . seqs)
  (apply semitone+ (* -12 num) seqs))

