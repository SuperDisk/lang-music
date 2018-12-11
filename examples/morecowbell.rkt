#lang music

(defseq bassline
  c r c r)
(defseq tuba_
  f8 g8. f8 g16)
(defseq harmon
  d#8 r8)
(defseq arpegg
  c16 d#16 f16 g16 a#16)
(dump-to-midi
 "more-cowbell.mid"
 (play
  ;; TODO: Fix this
  #;(together
   (octave+ 0 (with-voice cowbell
  (drumseq (loop 9999 a32))))
   (loop 20 bassline)
   #;(with-voice harmonica
     (loop 20 harmon))
   (loop 20 arpegg)
   (octave+ -3 (with-voice tuba (loop 20 tuba_))))))