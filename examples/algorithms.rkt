#lang racket

(require "../langmusic.rkt")

(require racket/generator)

(define *notes* (make-parameter empty))
(define nts (list a8 b8 c8 d8 e8 f8 g8))
(define (note n)
  (*notes* (cons (list-ref nts (modulo n (length nts))) (*notes*)))
  n)

(defun musicify (thk)
  (parameterize ((*notes* empty))
    (thk)
    (*notes*)))

(define (fib n)
  (cond
    ((= n 0) 1)
    ((= n 1) 1)
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

(dump-to-midi
 "pi.mid"
 (play >
  (musicify do-pi)))
