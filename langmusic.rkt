#lang racket

#|
.
. Nick Faro and Michael Gunantra Present:
.                                                                             $$
.    $$ $$  $$$$                                                              $$
.    $$ $$    $$
.   $$$$$$$   $$     $$$$   $$$$$    $$$$$          $$$$$$  $$  $$   $$$$$  $$$$     $$$$
.    $$ $$    $$        $$  $$  $$  $$  $$          $$ $ $$ $$  $$  $$        $$    $$  $$
.    $$ $$    $$        $$  $$  $$  $$  $$          $$ $ $$ $$  $$  $$        $$    $$
.    $$ $$    $$     $$$$$  $$  $$  $$  $$          $$ $ $$ $$  $$   $$$$     $$    $$
.   $$$$$$$   $$    $$  $$  $$  $$  $$  $$          $$ $ $$ $$  $$      $$    $$    $$
.    $$ $$    $$    $$  $$  $$  $$  $$  $$          $$ $ $$ $$  $$      $$    $$    $$  $$
.    $$ $$  $$$$$$   $$$$$  $$  $$   $$$$$          $$   $$  $$$$$  $$$$$   $$$$$$   $$$$
.                                       $$
.                                       $$
.                                   $$$$$
.
.    It's fun, it's easy, it's free
.
|#

(require (for-syntax syntax/parse racket/syntax
                     (only-in racket thunk*))
         (rename-in racket (> gt?) (< lt?) (>= gte?) (<= lte?))
         "voices.rkt")

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
                     ("16." .  ,(dot +sixteenth+))))))

(define current-channel 0)
(define (update-channel)
  (let ((new-chan (modulo (add1 current-channel) 16)))
    (if (= new-chan 10) ;; the midi percussion channel
        (set! current-channel (add1 new-chan))
        (set! current-channel new-chan))))

(define *octave* (make-parameter 0))
(define _voice_ (make-parameter #f))

(define (*voice*)
  (let ((o (_voice_)))
    (if (not o) 0 o)))

(define-syntax while
  (syntax-parser
    ((_ cnd body ...)
     #'(let recur ()
         (when cnd
           body ...
           (recur))))))

(define-syntax maybe-parameterize
  (syntax-parser
    ((_ (id val) code ...)
     #'(let ((c (thunk code ...)))
         (if (false? (id))
             (parameterize ((id val)) (c))
             (c))))))

(define funcall (curryr apply empty))

(define (repeat num ls)
  (build-list num (const ls)))

;; Makes a stream loop forever
(define (infinitize $)
  (let recur ((sub$ $))
    (cond
      ((stream-empty? sub$) (infinitize $))
      (else (stream-cons (stream-first sub$) (recur (stream-rest sub$)))))))

;; Maps a function over a sequence, accounting for togethered notes
(define (seqmap func seq)
  (stream-map (lambda (el)
                (if (list? el)
                    (map (curry seqmap func) el)
                    (func el)))
              seq))

(define (note-on midi-num note-length channel track)
  (let ((real-note (+ midi-num (* 12 (*octave*))))
        (duration (note-length->duration note-length)))
    (displayln (format "Playing note ~a, length: ~a, duration: ~a, octave: ~a, voice: ~a, channel: ~a, track: ~a"
                       real-note note-length duration (*octave*) (*voice*) channel track))
    (unless (= channel 10)
      (update-channel))))

(define (rest-on _ note-length channel track)
  (displayln (format "Resting on channel: ~a, track: ~a" channel track))
  (note-length->duration note-length)
  ;; We don't need to update channel...
  #;(unless (= channel 10)
    (update-channel)))

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
                               (stream (curry func
                                              midi-base
                                              #,real-duration)))
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

(define seq stream-append)
#;(define-syntax seq
  (syntax-parser
    ((_ seq ...)
     #'(stream-append seq ...))))

;; Groan. MIDI designates channel 10 to produce drum sounds rather than
;; regular voices, with seemingly no way to change this. drumseq is identical
;; to a regular seq, however it ignores whatever channel is passed to it and
;; just always plays on channel 10. Don't create any more than 1 drumseq or it will
;; likely sound like crap.
(define (drumseq . seqs)
  (define bigseq (apply stream-append seqs))
  (seqmap (lambda (n)
            (lambda (_channel track)
              (n 10 track)))
          bigseq))

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
    (seqmap (lambda (n)
              (lambda args
                (parameterize
                    ((*octave* (+ (*octave*) num)))
                  (apply n args))))
            bigseq)))

(define (with-voice v . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (seqmap (lambda (n)
              (lambda args
                (maybe-parameterize
                 (_voice_ v)
                 (apply n args))))
            bigseq)))

;; simple, huh
(define (together . seqs)
  (stream seqs))

;; Coroutine stuff

(define-syntax swap!
  (syntax-parser
    ((_ x:id func args ...)
     #'(let ((z (func x args ...)))
         (set! x z)
         z))))

(define (play . seqs)
  ;; The large sequence
  (define bigseq (apply stream-append seqs))

  (define (update-current-note cn)
    (cons (sub1 (car cn)) (cdr cn)))

  (define (note-done? n)
    (= (car n) 0))

  ;; Coroutine queue
  (define-struct coroutine (name k) #:transparent)
  (define hitlist empty)
  (define thread-queue empty)
  (define halt #f)

  (define (more-threads? tq)
    (gt? (length tq) 1))

  (define (current-continuation)
    (call-with-current-continuation
     (lambda (cc)
       (cc cc))))

  (define (spawn thunk #:pos (pos 'back))
    (let ((cc (current-continuation))
          (tname (gensym)))
      (if (procedure? cc)
          (begin
            (match pos
              ('back (set! thread-queue (append thread-queue (list (coroutine tname cc)))))
              ('front (set! thread-queue
                            (cons (car thread-queue)
                                  (cons (coroutine tname cc) (cdr thread-queue))))))
            tname)
          (begin (thunk)
                 (quit)))))

  (define (yield)
    (let ((cc (current-continuation)))
      (if (and (procedure? cc) (more-threads? thread-queue))
          (let ((me (car thread-queue))
                (next-thread (cadr thread-queue)))
            (set! thread-queue (append (cdr thread-queue) (list (make-coroutine (coroutine-name me) cc))))
            ((coroutine-k next-thread) 'resume))
          (void))))

  (define (quit)
    (if (more-threads? thread-queue)
        (let ((me (car thread-queue))
              (next-thread (cadr thread-queue)))
          (set! hitlist (remove (coroutine-name me) hitlist symbol=?))
          (set! thread-queue (cdr thread-queue))
          ((coroutine-k next-thread) 'resume))
        (halt)))

  (define (kill . names)
    (set! thread-queue
          (filter (lambda (cor)
                    (not (member (coroutine-name cor) names symbol=?)))
                  thread-queue)))

  ;; Gracefully ask a thread to die by adding its name to the hitlist
  (define (ask-to-die name)
    (set! hitlist (cons name hitlist)))

  (define (start-threads)
    (let ((cc (current-continuation)))
      (if cc
          (begin
            (set! halt (lambda () (cc #f)))
            (if (null? thread-queue)
                (void)
                (begin
                  (let ((next-thread (car thread-queue)))
                    #;(set! thread-queue (cdr thread-queue))
                    ((coroutine-k next-thread) 'resume)))))
          (void))))

  (define (thread $ track)
    (let recur (($ $))
      (cond
        ((and (or
               (member (coroutine-name (car thread-queue)) hitlist symbol=?)
               (stream-empty? $)))
         ;; If we need to die or, if our stream is empty, die
         (quit))
        (else
         ;; Process a new note
         (let ((note (stream-first $)))
           (cond
             ((list? note)
              ;; Spin up some subthreads for each substream
              (define subthreads
                (for/list ((sub$ note)
                           (tk (in-naturals (add1 track))))
                  (spawn (thunk (thread sub$ tk)) #:pos 'front)))
              ;; An extremely poor-man's version of a thread join.
              (define (alive? tname)
                (member tname thread-queue
                        #;(compose (curry equal? n) coroutine-name)
                        (lambda (n t) (equal? n (coroutine-name t)))))
              (define (all-still-alive?)
                (andmap alive? subthreads))
              (define (any-still-alive?)
                (ormap alive? subthreads))
              ;; As long as all of the subthreads are still alive,
              ;; we just do nothing. If one dies we exit this loop.
              (while (all-still-alive?)
                (yield))
              ;; Kill all coroutines that we spawned if one dies.
              (for-each ask-to-die subthreads)
              ;; We've asked all the threads to terminate, so once all note off
              ;; events have been emitted, then we can exit this following loop.
              (while (any-still-alive?)
                (yield))
              (recur (stream-rest $)))
             ((procedure? note)
              ;; Play the note and start counting down until it dies
              (let* ((dur (note current-channel track)))
                (yield)
                (recur (stream-rest $))))
             (else (error (format "Got an invalid note! ~a" note)))))))))

  ;;Spawn the root thread.
  (define root (spawn (thunk (thread bigseq 0))))
  (start-threads))

(define > (stream (thunk (displayln "Bumping octave up"))))
(define < (stream (thunk (displayln "Bumping octave down"))))

(defseq bassline
  c4 r4)

(defun random-phrase (num-notes)
  (let* ((ls (list a b c d e f g))
         (len (length ls)))
    (build-list num-notes (thunk* (list-ref ls (random len))))))

(defseq phrase1
  c4 b4)

(define doplay
  (thunk
   (play
    (together
     (drumseq a a a a16)
     (with-voice overdriven-guitar
       (loop 0 bassline))
     #;(octave+ 1 (octave+ 3 (seq b4 b4)))
     #;(seq a8 a8 a8 a8)))))
#;
(define doplay
  (thunk
   (play
    (together
     (loop 2 phrase1)
     (loop 0 bassline))
    (together
     (loop 2 (octave+ 1 phrase1))
     (loop 0 (octave+ 1 bassline))))))
