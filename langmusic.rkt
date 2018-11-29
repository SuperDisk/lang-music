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
         racket/pretty
         binaryio
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

(define +num-channels+ 16)
(define +drum-track+ 9) ;; zero indexed

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
  (let ((new-chan (modulo (add1 current-channel) +num-channels+)))
    (if (= new-chan +drum-track+) ;; the midi percussion channel
        (set! current-channel (add1 new-chan))
        (set! current-channel new-chan))))

(define *octave* (make-parameter 0))
(define _voice_ (make-parameter #f))
(define (*voice*)
  (let ((o (_voice_)))
    (if (not o) 0 o)))
(define *trackdata* (make-parameter #f))

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
    (*trackdata* (hash-update (*trackdata*) track
                              (lambda (trk-data)
                                `(0 (off ,real-note ,channel) ,duration (on ,real-note ,channel ,(*voice*)) . ,trk-data))))
    (unless (= channel +drum-track+)
      (update-channel))))

(define (rest-on _midi-num note-length _channel track)
  (define duration (note-length->duration note-length))
  (displayln (format "Resting on track: ~a" track))
  (*trackdata* (hash-update (*trackdata*) track
                            (lambda (trk-data)
                              ;; There's a pass later that combines all numbers that are next to each other.
                              `(,duration . ,trk-data))))
  (note-length->duration note-length))

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

;; Groan. MIDI designates channel 10 to produce drum sounds rather than
;; regular voices, with seemingly no way to change this. drumseq is identical
;; to a regular seq, however it ignores whatever channel is passed to it and
;; just always plays on channel 10. Don't create any more than 1 drumseq or it will
;; likely sound like crap.
(define (drumseq . seqs)
  (define bigseq (apply stream-append seqs))
  (seqmap (lambda (n)
            (lambda (_channel track)
              (n +drum-track+ track)))
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
                (parameterize ((*octave* (+ (*octave*) num)))
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
  (parameterize ((*trackdata*
                  (for/hash ((i +num-channels+))
                    (values i '(0)))))
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
                ;; They're finally all dead. They all theoretically should have
                ;; emitted the same number of notes (TODO: fix this) so just
                ;; see how many that was and then emit a rest that lasts that long.
                (define first-subtrack (add1 track))
                (define subtrack-data (hash-ref (*trackdata*) first-subtrack))
                (define rest-duration (apply + (filter number? subtrack-data)))
                (*trackdata* (hash-update (*trackdata*) track
                                            (lambda (trk-data)
                                              `(,rest-duration . ,trk-data))))
                (recur (stream-rest $)))
               ((procedure? note)
                ;; Play the note and start counting down until it dies
                (let* ((dur (note current-channel track)))
                  (yield)
                  (recur (stream-rest $))))
               (else (error (format "Got an invalid note! ~a" note)))))))))

    ;;Spawn the root thread.
    (define root (spawn (thunk (thread bigseq 0))))
    (start-threads)

    (define (finalize-track trk)
      (foldl (lambda (n r)
               (if (and (not (empty? r)) (number? n) (number? (car r)))
                   (cons (+ n (car r)) (cdr r))
                   (cons n r)))
             empty (cons 'end-of-track trk)))

    (pretty-print (*trackdata*))
    (define final-track-data
      (for/hash (((k v) (in-hash (*trackdata*)))
                 #:when (not (equal? v '(0))))
        (values k (finalize-track v) #;(cons 'end-of-track (reverse (cdr v))))))
    (pretty-print final-track-data)
    'yowza!
    (dump-to-midi "output.mid" final-track-data)))

(define (bin num size)
  (integer->bytes num size #f))

(define (sbin num size)
  (integer->bytes num size #t))

(define (dump-header-chunk numtracks)
  (bytes-append
   #"MThd"
   (bin 6 4) ;; Always 6 for MIDI 1.0
   (bin 1 2) ;; What type of MIDI (we output Format 1)
   (bin numtracks 2) ;; Number of tracks
   (sbin +ticks-per-quarter+ 2) ;; How many ticks:quarter
   ))

;; Ported from midi.lisp
;; seems like Racket's bit twiddling features are inspired
;; by CL's!
(define (write-variable-length-quantity quantity (termination 0))
  (when (gt? quantity 127)
    (write-variable-length-quantity
     (arithmetic-shift quantity -7) #x80))
  (write-bytes (bytes
                (bitwise-ior
                 (bitwise-and quantity #x7f)
                 termination))))

(define (vlq n)
  (with-output-to-bytes
    (thunk (write-variable-length-quantity n))))

(define (dump-track data)
  (define chunked
    (let recur ((data data))
      (if (empty? data)
          empty
          (cons
           (cons (car data) (cadr data))
           (recur (cddr data))))))

  (define (pair->binary pair)
    (define (statusbyte sb val)
      (bytes
       (bitwise-ior
        (arithmetic-shift sb 4)
        val)))
    (match pair
      (`(,del . ,event)
       (bytes-append
        ;;Delta time
        (vlq del)
        ;;The midi event data
        (match event
          (`(on ,midi-num ,channel ,voice)
           (bytes-append
            (statusbyte #x9 channel)
            (bytes midi-num)
            (bytes 40) ;; Do this for non vel-sensitive???
            ))
          (`(off ,midi-num ,channel)
           (bytes-append
            (statusbyte #x8 channel)
            (bytes midi-num)
            (bytes 40) ;; Do this for non vel-sensitive???
            ))
          ('end-of-track (bytes #xFF #x2F #x00)) ;; It's *NOT* optional!!!!!!!!!!! lmao!
          (`(no-op) (error "not supported yet")))))))

  (define track-data (apply bytes-append
                            (map pair->binary chunked)))
  (bytes-append
   #"MTrk"
   (bin (bytes-length track-data) 4)
   track-data))

(define (dump-first-track)
  #;(define settempo
    (bytes-append
     (bytes #xFF
            #x51
            #x03)
     (bin 500000 3)))
  #;(define timesig
    (bytes-append
     (bytes #xFF
            #x58
            #x04

            4 ;; numerator
            2 ;; denominator^2
            24 ;; clocks???
            8 ;; 1/32 notes per 24 midi clocks (Wtf)
            )))
  (define final-body
    (bytes-append
     #;(vlq 0)
     #;settempo
     #;(vlq 0)
     #;timesig))
  (bytes-append
   #"MTrk"
   (bin (bytes-length final-body) 4)
   final-body))

(define (dump-to-midi path trackdata)
  (pretty-print trackdata)
  (define non-empty-tracks
    (for/list (((n tk) (in-hash trackdata))
               #:when (not (empty? tk)))
      tk))
  (displayln (length non-empty-tracks))
  (with-output-to-file path #:exists 'replace
    (thunk
     (write-bytes (dump-header-chunk (length non-empty-tracks)))
     ;; The special first track.
     #;(write-bytes (dump-first-track))
     (for ((dat non-empty-tracks))
       (write-bytes (dump-track dat))))))

(define > (stream (thunk (displayln "Bumping octave up"))))
(define < (stream (thunk (displayln "Bumping octave down"))))

(defseq bassline
  c4 d#4)

(defun random-phrase (num-notes)
  (let* ((ls (list a b c d e f g))
         (len (length ls)))
    (build-list num-notes (thunk* (list-ref ls (random len))))))

(defseq phrase1
  c4 f4)

(define doplay
  (thunk
   (play
    (octave+ 3
             (loop 3 c8 d#8 f8 g8))
    #;(together
     (seq a b c)
     (seq d e f)))
   #;(play
    (together
     (loop 3 phrase1)
     (loop 3 (octave+ 2 bassline))))))
(doplay)

#;(define doplay
  (thunk
   (play
    (together
     (loop 2 phrase1)
     (loop 0 bassline))
    (together
     (loop 2 (octave+ 1 phrase1))
     (loop 0 (octave+ 1 bassline))))))
