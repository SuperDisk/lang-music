#lang racket

(require (for-syntax syntax/parse racket/syntax racket/generator)
         racket/generator
         compatibility/defmacro)

(define octave (make-parameter 0))
(define voice (make-parameter 0))

(define (repeat num ls)
  (build-list num (const ls)))

;; We have to define our own custom stream functions because the built-in Racket ones
;; don't let you have "interesting" tails.
(define empty$ null)
(define empty$? null?)
(define-syntax-rule (cons$ x y)
  (cons x (delay y)))
(define car$ car)
(define (cdr$ $) (force (cdr $)))
(define (infinitize $)
  (let recur ((sub$ $))
    (cond
      ((empty$? sub$) (infinitize $))
      (else (cons$ (car$ sub$) (recur (cdr$ sub$)))))))

(define (append$ . strs)
  (define (_append2$ $1 $2)
    (cond
      ((empty$? $1) $2)
      ((empty$? $2) $1)
      (else
       (cons$ (car$ $1) (append$ (cdr$ $1) $2)))))
  (foldr _append2$ empty$ strs))
(define (take$ $ n)
  (cond
    ((zero? n) empty$)
    (else (cons (car$ $) (take$ (cdr$ $) (sub1 n))))))
(define (stream$ . items)
  (let stream$^ ((items items))
    (cond
      ((empty? items) empty$)
      (else (cons$ (car items) (stream$^ (cdr items)))))))
(define stream$* (curry apply stream$))

(define (play-note midi-num duration)
  (let ((real-note (+ midi-num (* 12 (octave)))))
    (displayln (format "Playing note ~a, duration: ~a, octave: ~a, voice: ~a"
                       real-note duration (octave) (voice)))))

(define-for-syntax (make-note-symbol s1 s2)
  (string->symbol
   (string-append (symbol->string s1) s2)))

(define-syntax define-note-divisions
  (syntax-parser
    ((_ note midi-base)
     (let* ((note-base (syntax->datum #'note))
            (defs (map
                   (lambda (duration)
                     (let ((real-duration
                            (if (string=? duration "") "0" duration)))
                       (with-syntax ((note-id
                                      (datum->syntax
                                       #'note
                                       (make-note-symbol note-base duration))))
                         #`(begin
                             (define note-id
                               (stream$ (thunk (play-note
                                                midi-base
                                                #,(string->number real-duration)))))
                             (provide note-id)))))
                   '("" "2" "4" "8" "16" "32"))))
       #`(begin
           #,@defs)))))

(define-syntax defseq
  (syntax-parser
    ((_ name seq ...)
     #`(define name
         (append$ seq ...)))))

(define (loop num . seqs)
  (let ((bigseq (apply append$ seqs)))
    (cond
      ((zero? num) (infinitize bigseq))
      ((positive? num) (apply append$ (repeat num bigseq)))
      (else (error "Can't loop with a negative number!")))))

(define-note-divisions g# 32)
(define-note-divisions g 31)
(define-note-divisions f# 30)
(define-note-divisions f 29)
(define-note-divisions e 28)
(define-note-divisions d# 27)
(define-note-divisions d 26)
(define-note-divisions c# 25)
(define-note-divisions c 24)
(define-note-divisions b 23)
(define-note-divisions a# 22)
(define-note-divisions a 21)

(defseq phrase1 a b c d e d# g)
