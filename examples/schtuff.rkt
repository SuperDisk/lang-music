#lang racket

(require "langmusic.rkt")

(defseq arpeggio24
  e32
  g32
  b32
  e32
  e32
  g32
  b32
  e32
  e32
  g32
  b32
  e32
  f#32
  a32
  b32
  e32
  f#32
  a32
  b32
  e32
  f#32
  a32
  b32
  e32
  f#32
  a32
  b32
  e32
  f#32
  a32
  b32
  e32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32
  g32
  b32
  e32
  g32)

(defseq bass24
  f#8. > f#8. < b8 > c#8.
  e8 e16 < g#8 a8. >
  a8. < b16 > c16
  c#8 e8. < b16 > c#8 )

(defseq lead1
  r4 f4 e4 c8. d4 r4 g16 g#16 a8 > a8. < g8)

(defseq lead2
  d4 d8 d16 e8 c8. d r)

(defun fact (n)
  (let ((notes empty)
        (chord (list c16 d#16 f16 g16)))
    (let recur ((n n))
      (if (= n 0)
          (begin (set! notes (cons c16 notes))
                 1)
          (begin
            (let ((output (* n (recur (sub1 n)))))
              (set! notes (cons (list-ref chord (modulo output (length chord)))
                                notes))
              output))))
    notes))

(dump-to-midi
 "output.mid"
 (play
  a
  b
  c
  (loop 5 (fact 5))
  d
  e
  f
  #;(together

   #;(seq lead1 lead2 lead1 lead2)
   #;(seq
    (myseq r)
    (loop 2 bass24)
    (octave+ 1 (with-voice rock-organ (loop 2 bass24)))))))
