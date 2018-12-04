#lang racket

(require "../langmusic.rkt")

(defseq bassline
  c d# f g)

(defun random-phrase (num-notes)
  (let* ((ls (list a8 b8 c8 d8 e8 f8 g8))
        (len (length ls)))
    (build-list num-notes (thunk* (list-ref ls (random len))))))

(defseq phrase1
  c d e f g4 a16 b16 > (random-phrase 16) <)

(dump-to-midi
 "from-the-slide.mid"
 (play
  phrase1))