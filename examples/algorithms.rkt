#lang racket

(require "../langmusic.rkt")

(require racket/generator)

(define c-major (list c8 d8 e8 f8 g8 a8 b8))
(define d-major (list d8 e8 f#8 g8 a8 b8 c#8 d8))
(define e-major (list e8 f#8 g#8 a8 b8 c#8 d#8 e8))
(define f-major (list f8 g8 a8 a#8 c8 d8 e8 f8))
(define g-major (list g8 a8 b8 c8 d8 e8 f#8 g8))
(define a-major (list a8 b8 c#8 d8 e8 f#8 g#8 a8))
(define b-major (list b8 c#8 d#8 e8 f#8 g#8 a#8 b8))
(define c#-major (list c#8 d#8 f8 f#8 g#8 a#8 c8 c#8))
(define d#-major (list d#8 f8 g8 g#8 c#8 c8 d8 d#8))
(define f#-major (list f#8 g#8 a#8 b8 c#8 d#8 f8 f#8))
(define g#-major (list g#8 a#8 c8 c#8 d#8 f8 g8 g#8))
(define a#-major (list a#8 c8 d8 d#8 f8 g8 a8 a#8))
(define bigscale (append c-major 
                         d-major 
                         e-major 
                         f-major 
                         g-major 
                         a-major 
                         b-major 
                         c#-major
                         d#-major
                         f#-major
                         g#-major
                         a#-major
                         ))

(define *notes* (make-parameter #f))
(define *nts* (make-parameter #f))
(define (note n)
  (*notes* (cons (list-ref (*nts*) (modulo n (length (*nts*)))) (*notes*)))
  n)

(defun musicify (thk nts)
  (parameterize ((*notes* empty)
                 (*nts* nts))
    (thk)
    (*notes*)))

(define (fib n)
  (cond
    ((= n 0) (note 1))
    ((= n 1) (note 1))
    (else
     (note
      (+ (fib (sub1 n)) (fib (- n 2)))))))

(define (ack m n)
  (cond
    ((zero? m) (add1 n))
    ((zero? n) (note (ack (sub1 m) 1)))
    (else (note (ack (sub1 m)
                     (note (ack m (sub1 n))))))))

(define pidig
  (generator ()
             (let loop ([q 1] [r 0] [t 1] [k 1] [n 3] [l 3])
               (if (lt? (- (+ r (* 4 q)) t) (* n t))
                   (begin (yield n)
                          (loop (* q 10) (* 10 (- r (* n t))) t k
                                (- (quotient (* 10 (+ (* 3 q) r)) t) (* 10 n))
                                l))
                   (loop (* q k) (* (+ (* 2 q) r) l) (* t l) (+ 1 k)
                         (quotient (+ (* (+ 2 (* 7 k)) q) (* r l)) (* t l))
                         (+ l 2))))))

(define (do-pi)
  (for ((_ (in-range 30)))
    (note (pidig))))
#;
(begin
(dump-to-midi
 "algos-together.mid"
 (play
  (together
   (with-voice harpsichord (musicify (thunk (ack 2 3)) d#-major))
   (with-voice rock-organ (musicify (thunk (fib 8)) d-major))
   (with-voice harmonica (musicify do-pi g-major)))))

(dump-to-midi
 "ackermann.mid"
 (play > (musicify (thunk (ack 2 3)) d#-major)))

(dump-to-midi
 "fibonacci.mid"
 (play > (musicify (thunk (fib 8)) c-major))))

(dump-to-midi
 "pi.mid"
 (play > (musicify do-pi bigscale)))
