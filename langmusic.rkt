#lang racket

(require (for-syntax syntax/parse racket/syntax))

;; Some structures:
#|

|#

;; You can fit 8 32nd notes in a quarter note.
(define +ticks-per-quarter+ 8)

(define *octave* (make-parameter 0))
(define *voice* (make-parameter 0))

;; The ticker returns the number of ticks to wait until the next note.
(define *ticker* (make-parameter identity))

(define funcall (curryr apply empty))

(define (repeat num ls)
  (build-list num (const ls)))

(define (infinitize $)
  (let recur ((sub$ $))
    (cond
      ((stream-empty? sub$) (infinitize $))
      (else (stream-cons (stream-first sub$) (recur (stream-rest sub$)))))))

(define (stream-take $ n)
  (cond
    ((zero? n) '())
    (else (cons (stream-first $) (stream-take (stream-rest $) (sub1 n))))))

(define (note-on midi-num duration)
  (let ((real-note (+ midi-num (* 12 (*octave*)))))
    (displayln (format "Playing note ~a, duration: ~a, octave: ~a, voice: ~a"
                       real-note duration (*octave*) (*voice*)))))

(define-syntax punk
  (syntax-parser
    ((_ ((id val) ...) code ...)
     #'(thunk
        (parameterize ((id val) ...)
          code ...)))))

(define-syntax phunk
  (syntax-parser
    ((_ (id val) ...)
     #'(lambda (thk)
         (thunk
          (parameterize ((id val) ...)
            (thk)))))))

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
                               (stream (thunk (note-on
                                                midi-base
                                                #,(string->number real-duration)))))
                             (provide note-id)))))
                   '("" "2" "4" "8" "16" "32"))))
       #`(begin
           #,@defs)))))

(define-syntax defun
  (syntax-parser
    ((_ name (id ...) code ...)
     #'(define (name id ...)
         (let* ((seqs (begin code ...)))
           (apply stream-append seqs))))))

(define-syntax defseq
  (syntax-parser
    ((_ name seq ...)
     #`(define name
         (stream-append seq ...)))))

(define (loop num . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (cond
      ((zero? num) (infinitize bigseq))
      ((positive? num) (apply stream-append (repeat num bigseq)))
      (else (error "Can't loop with a negative number!")))))

(define (octave+ num . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (stream-map identity bigseq)))

(define-syntax together
  (syntax-parser
    ((_ seq ...)
     (let* ((seqs (syntax->list #'(seq ...)))
            (syms (build-list (length seqs) (lambda _ (datum->syntax #'here (gensym)))))
            (pairs (map (lambda (seq sym) #`(#,sym #,seq)) seqs syms)))
       #`(for/stream #,(datum->syntax #'here pairs)
                     (list #,@syms))))))

(define (play . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (for ((note bigseq))
      (cond
        ((list? note) (for-each funcall note))
        (else (note))))))

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

(define > (stream (thunk (displayln "Bumping octave up"))))
(define < (stream (thunk (displayln "Bumping octave down"))))

(defseq bassline
  c d# f g)

(defun random-phrase (num-notes)
  (let* ((ls (list a b c d e f g))
        (len (length ls)))
    (build-list num-notes (thunk* (list-ref ls (random len))))))

(defseq phrase1
  c d e f g4 a16 b16 > (random-phrase 8) <)

(play
 (together
  (loop 2 phrase1)
  (loop 0 bassline))
 (together
  (loop 2 (octave+ 1 phrase1))
  (loop 0 (octave+ 1 bassline))))
