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

(define *octave* (make-parameter 3))
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

;; `note-on` and `rest-on` produce lists of "events" and numbers that will become
;; midi events and delta times. They can produce multiple numbers in a row
;; as these will be added together later on in `play`.
(define (note-on midi-num note-length channel track)
  (let ((real-note (+ midi-num (* 12 (*octave*))))
        (duration (note-length->duration note-length)))
    #;(displayln (format "Playing note ~a, length: ~a, duration: ~a, octave: ~a, voice: ~a, channel: ~a, track: ~a"
                       real-note note-length duration (*octave*) (*voice*) channel track))
    (define retval
      `(0 (off ,real-note ,channel) ,duration (on ,real-note ,channel ,(*voice*))))
    (unless (= channel +drum-track+)
      (update-channel))

    retval))

(define (rest-on _midi-num note-length _channel track)
  (define duration (note-length->duration note-length))
  (displayln (format "Resting on track: ~a" track))
  (list duration))

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

;; Notes for playback
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
(define-note-divisions p #f rest-on) ;; BASIC had this.

;; Setting the octave
(define o0 (thunk* (*octave* 0) empty))
(define o1 (thunk* (*octave* 1) empty))
(define o2 (thunk* (*octave* 2) empty))
(define o3 (thunk* (*octave* 3) empty))
(define o4 (thunk* (*octave* 4) empty))
(define o5 (thunk* (*octave* 5) empty))

(provide o0 o1 o2 o3 o4 o5)

(define-syntax defun
  (syntax-parser
    ((_ name (id ...) code ...)
     #'(define (name id ...)
         (let* ((seqs (let () code ...)))
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
    ((_ name s ...)
     #`(define name
         (seq s ...)))))

(define (loop num . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (cond
      ((zero? num) (infinitize bigseq))
      ((positive? num) (apply stream-append (repeat num bigseq)))
      (else (error "Can't loop with a negative number!")))))

(define (semitone+ num . seqs)
  (define (bump-up evt)
    (match evt
      (`(off ,note ,chn)
       `(off ,(+ note num) ,chn))
      (`(on ,note ,chn ,voice)
       `(on ,(+ note num) ,chn ,voice))
      (x x)))

  (let ((bigseq (apply stream-append seqs)))
    (seqmap (lambda (n)
              (lambda args
                (define note-out (apply n args))
                (map bump-up note-out)))
            bigseq)))

(define (semitone- num . seqs)
  (apply semitone+ (- num) seqs))

(define (octave+ num . seqs)
  (apply semitone+ (* 12 num) seqs))

(define (octave- num . seqs)
  (apply octave+ (- num) seqs))

;; doesn't work that well...
(define (force-length num . seqs)
  (define (bump-up evt)
    (match evt
      (x #:when (number? x) num)
      (x x)))

  (let ((bigseq (apply stream-append seqs)))
    (seqmap (lambda (n)
              (lambda args
                (define note-out (apply n args))
                (map bump-up note-out)))
            bigseq)))

(define (with-voice v . seqs)
  (let ((bigseq (apply stream-append seqs)))
    (seqmap (lambda (n)
              (lambda args
                (maybe-parameterize
                 (_voice_ v)
                 (apply n args))))
            bigseq)))

(define (repeat num ls)
  (build-list num (const ls)))

;; simple, huh
(define (together . seqs)
  (stream seqs))

(define (play . seqs)
  (parameterize ((*trackdata*
                  (for/hash ((i +num-channels+))
                    (values i '(0)))))
    ;; The large sequence
    (define bigseq (apply stream-append seqs))

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
                (define get-time (compose (curry apply +) (curry filter number?)))

                ;; Spin up some subthreads for each substream
                (define sleep-before (get-time (hash-ref (*trackdata*) track)))

                (define subthreads-alist
                  (for/list ((sub$ note)
                             (tk (in-naturals (add1 track))))
                    ;; Add sleep to the new tracks until they reach our amount
                    (define already-slept (get-time (hash-ref (*trackdata*) tk)))
                    (*trackdata* (hash-update (*trackdata*) tk
                                              (lambda (trk-data)
                                                `(,(- sleep-before already-slept)
                                                  .
                                                  ,trk-data))))
                    (cons
                     (spawn (thunk (parameterize ((*octave* (*octave*)))
                                     (thread sub$ tk)))
                            #:pos 'front)
                     tk)))
                (define subthreads (map car subthreads-alist))
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
                (define (thread->data th)
                  (hash-ref (*trackdata*)
                            (cdr (assv th subthreads-alist))))
                ;; One has died. Find how much time it spent and start killing
                ;; once that count or greater appears in the others.
                (define first-dead
                  (first (filter (negate alive?) subthreads)))
                (define first-dead-data (thread->data first-dead))
                (define time-spent (get-time first-dead-data))
                (while (any-still-alive?)
                  (for ((th subthreads)
                        #:when (not (member th hitlist)))
                    (define data (thread->data th))
                    (define generated-time (get-time data))
                    (when (>= generated-time time-spent)
                      (ask-to-die th)))
                  (yield))
                ;; They're finally all dead. They all theoretically should have
                ;; emitted the same number of time (TODO: fix this) so just
                ;; see how many that was and then emit a rest that lasts that long.
                (define biggest-time
                  (apply max (map (compose get-time thread->data) subthreads)))
                (*trackdata* (hash-update (*trackdata*) track
                                            (lambda (trk-data)
                                              `(,biggest-time . ,trk-data))))
                (recur (stream-rest $)))
               ((procedure? note)
                ;; Play the note, yield to other threads, then continue processing
                (let ((note-out (note current-channel track)))
                  (*trackdata* (hash-update (*trackdata*)
                                            track
                                            (curry append note-out)))
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

    #;(pretty-print (*trackdata*))
    (define final-track-data
      (for/hash (((k v) (in-hash (*trackdata*)))
                 #:when (not (equal? v '(0))))
        (values k (finalize-track v) #;(cons 'end-of-track (reverse (cdr v))))))
    ;; (pretty-print final-track-data)
    ;; 'yowza!
    #;(dump-to-midi "output.mid" final-track-data)
    final-track-data))

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
   (sbin +ticks-per-quarter+ 2)))

;; Ported from midi.lisp
;; seems like Racket's bit twiddling features are inspired
;; by CL's
(define (vlq n)
  (define (write-variable-length-quantity quantity (termination 0))
    (when (gt? quantity 127)
      (write-variable-length-quantity
       (arithmetic-shift quantity -7) #x80))
    (write-bytes (bytes
                  (bitwise-ior
                   (bitwise-and quantity #x7f)
                   termination))))
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
            ;; do the program change every time (bad!)
            (statusbyte #xC channel)
            (bytes voice)
            (vlq 0)
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

(define (dump-to-midi path trackdata)
  #;(pretty-print trackdata)
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

(define > (stream (thunk* (*octave* (add1 (*octave*))) empty)))
(define < (stream (thunk* (*octave* (sub1 (*octave*))) empty)))

(provide defun seq drumseq defseq loop octave+ semitone+ with-voice
         together play > < lt? gt? dump-to-midi #;force-length
         (all-from-out "voices.rkt"))
