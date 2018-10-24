#lang racket

; thread-queue : list[continuation]
(define thread-queue '())

; halt : continuation
(define halt #f)

; current-continuation : -> continuation
(define (current-continuation)
  (call-with-current-continuation
   (lambda (cc)
     (cc cc))))

; spawn : (-> anything) -> void
(define (spawn thunk)
  (let ((cc (current-continuation)))
    (if (procedure? cc)
        (set! thread-queue (append thread-queue (list cc)))
        (begin (thunk)
               (quit)))))

; yield : value -> void
(define (yield)
  (let ((cc (current-continuation)))
    (if (and (procedure? cc) (pair? thread-queue))
        (let ((next-thread (car thread-queue)))
          (set! thread-queue (append (cdr thread-queue) (list cc)))
          (next-thread 'resume))
        (void))))

; quit : -> ...
(define (quit)
  (if (pair? thread-queue)
      (let ((next-thread (car thread-queue)))
        (set! thread-queue (cdr thread-queue))
        (next-thread 'resume))
      (halt)))
   
; start-threads : -> ...
(define (start-threads)
  (let ((cc (current-continuation)))
    (if cc
        (begin
          (set! halt (lambda () (cc #f)))
          (if (null? thread-queue)
              (void)
              (begin
                (let ((next-thread (car thread-queue)))
                  (set! thread-queue (cdr thread-queue))
                  (next-thread 'resume)))))
        (void))))




;; Example cooperatively threaded program
(define counter 10)

(define (make-thread-thunk name)
  (letrec ((loop (lambda ()
                   (when (< counter 0)
                     (quit))
                   (display "in thread ")
                   (display name)
                   (display "; counter = ")
                   (display counter)
                   (newline)
                   (set! counter (- counter 1))
                   (yield)
                   (loop))))
    loop))

(define (looper)
  (let recur ((n 6))
    (if (not (= n 0))
        (begin
          (displayln (format "Loopin' ~a" n))
          (yield)
          (recur (sub1 n)))
        (displayln "Looping is done."))))

(define (waiter)
  (let/cc k
    (let recur ()
      (if (not (< (length thread-queue) 1))
          (begin 
            (displayln "Waiting...")
            (yield)
            (recur))
          (displayln "I'm the only one left! Bye.")))))
    

#;(spawn (make-thread-thunk 'a))
#;(spawn (make-thread-thunk 'b))
#;(spawn (make-thread-thunk 'c))

(spawn looper)
(spawn waiter)

(start-threads)