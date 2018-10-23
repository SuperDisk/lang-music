#lang racket

#|

   Nick Faro and Michael Gunantra Present:
                                                                             ;;
    ;; ;;  ;;;;                                    ;;   ;;                   ;;
    ;; ;;    ;;                                    ;;   ;;
   ;;;;;;;   ;;     ;;;;   ;;;;;    ;;;;;          ;;; ;;; ;;  ;;   ;;;;;  ;;;;     ;;;;
    ;; ;;    ;;        ;;  ;;  ;;  ;;  ;;          ;; ; ;; ;;  ;;  ;;        ;;    ;;  ;;
    ;; ;;    ;;        ;;  ;;  ;;  ;;  ;;          ;; ; ;; ;;  ;;  ;;        ;;    ;;
    ;; ;;    ;;     ;;;;;  ;;  ;;  ;;  ;;          ;; ; ;; ;;  ;;   ;;;;     ;;    ;;
   ;;;;;;;   ;;    ;;  ;;  ;;  ;;  ;;  ;;          ;;   ;; ;;  ;;      ;;    ;;    ;;
    ;; ;;    ;;    ;;  ;;  ;;  ;;  ;;  ;;          ;;   ;; ;;  ;;      ;;    ;;    ;;  ;;
    ;; ;;  ;;;;;;   ;;;;;  ;;  ;;   ;;;;;          ;;   ;;  ;;;;;  ;;;;;   ;;;;;;   ;;;;
                                       ;;
                                       ;;
                                   ;;;;;

   It's fun, it's easy, it's free

|#

(require (for-syntax syntax/parse racket/syntax
                     (only-in racket thunk*)))

;; You can fit 8 32nd notes in a quarter note.
(define +ticks-per-quarter+ 8)
(define +whole+        (* +ticks-per-quarter+ 4))
(define +half+         (* +ticks-per-quarter+ 2))
(define +quarter+         +ticks-per-quarter+)
(define +eighth+       (/ +ticks-per-quarter+ 2))
(define +sixteenth+    (/ +ticks-per-quarter+ 4))
(define +thirtysecond+ (/ +ticks-per-quarter+ 8))
(define (dot num) (+ num (/ num 2)))

(define note-length->duration
  (compose cdr
           (curryr assv
                   `(("0" .  ,+whole+)
                     ("2" .  ,+half+)
                     ("4" .  ,+quarter+)
                     ("8" .  ,+eighth+)
                     ("16" . ,+sixteenth+)
                     ("32" . ,+thirtysecond+)

                     ("0." .  ,(dot +whole+))
                     ("2." .  ,(dot +half+))
                     ("4." .  ,(dot +quarter+))
                     ("8." .   ,(dot +eighth+))
                     ("16." .  ,(dot +sixteenth+))
                     ;; we don't have dotted 32nd notes.
                     ))))

(define *octave* (make-parameter #f))
(define *voice* (make-parameter #f))

;; The ticker returns the number of ticks to wait until the next note.
(define *ticker* (make-parameter identity))

(define (octave)
  (let ((o (*octave*)))
    (if (not o) 0 o)))

(define (voice)
  (let ((o (*voice*)))
    (if (not o) 0 o)))

(define-syntax maybe-parameterize
  (syntax-parser
    ((_ (id val) code ...)
     #'(if (false? (id))
           (parameterize ((id val))
             code ...)
           (begin
             code ...)))))

(define funcall (curryr apply empty))

(define (repeat num ls)
  (build-list num (const ls)))

;; Makes a stream loop forever
(define (infinitize $)
  (let recur ((sub$ $))
    (cond
      ((stream-empty? sub$) (infinitize $))
      (else (stream-cons (stream-first sub$) (recur (stream-rest sub$)))))))

(define (stream-take $ n)
  (cond
    ((zero? n) '())
    (else (cons (stream-first $) (stream-take (stream-rest $) (sub1 n))))))

;; Maps a function over a sequence, accounting for togethered notes
(define (seqmap func seq)
  (stream-map (lambda (el)
                (if (list? el)
                    (map func el)
                    (func el)))
              seq))

(define (note-on midi-num note-length)
  (let ((real-note (+ midi-num (* 12 (octave))))
        (duration (note-length->duration note-length)))
    (displayln (format "Playing note ~a, duration: ~a, octave: ~a, voice: ~a"
                       real-note duration (octave) (voice)))
    (thunk
     (format "Turning off note ~a, duration: ~a, octave: ~a, voice: ~a"
             real-note duration (octave) (voice)))))

(define (rest-on _ duration)
  (displayln "Resting"))

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
    ((_ note midi-base) #'(define-note-divisions note midi-base note-on))
    ((_ note midi-base func)
     (let* ((note-base (syntax->datum #'note))
            (defs (map
                   (lambda (duration)
                     (let ((real-duration
                            (cond
                              ((string=? duration "") "0")
                              ((string=? duration ".") "0.")
                              (else duration))))
                       (with-syntax ((note-id
                                      (datum->syntax
                                       #'note
                                       (make-note-symbol note-base duration))))
                         #`(begin
                             (define note-id
                               (stream (thunk (func
                                                midi-base
                                                #,real-duration))))
                             (provide note-id)))))
                   '("" "2" "4" "8" "16" "32" "." "2." "4." "8." "16." "32."))))
       #`(begin
           #,@defs)))))

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
;; Rest
(define-note-divisions r #f rest-on)

(define-syntax defun
  (syntax-parser
    ((_ name (id ...) code ...)
     #'(define (name id ...)
         (let* ((seqs (begin code ...)))
           (apply stream-append seqs))))))

(define-syntax seq
  (syntax-parser
    ((_ seq ...)
     #'(stream-append seq ...))))

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
            (syms (build-list (length seqs) (thunk* (datum->syntax #'here (gensym)))))
            (pairs (map (lambda (seq sym) #`(#,sym #,seq)) seqs syms)))
       #`(for/stream #,(datum->syntax #'here pairs)
                     (list #,@syms))))))

(define (play . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (for ((note bigseq))
      (cond
        ((list? note) (for-each funcall note))
        (else (note))))))


(define > (stream (thunk (displayln "Bumping octave up"))))
(define < (stream (thunk (displayln "Bumping octave down"))))

(defseq bassline
  c2)

(defun random-phrase (num-notes)
  (let* ((ls (list a b c d e f g))
        (len (length ls)))
    (build-list num-notes (thunk* (list-ref ls (random len))))))

(defseq phrase1
  c4 b4)

(play
 (together
  (loop 2 phrase1)
  (loop 0 bassline))
 (together
  (loop 2 (octave+ 1 phrase1))
  (loop 0 (octave+ 1 bassline))))
