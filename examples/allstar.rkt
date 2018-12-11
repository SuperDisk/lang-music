#lang music

(define alls
  "c+<a+a+4g+f+f+b4a+a+g+g+f+4f+O4C+O3a+a+g+g+f+f+d+4c+2&c+f+f+O4C+O3a+a+g+g+f+f+b4a+a+g+g+f+f+>c+4<a+a+g+4f+f+g+4d+2&d+f+.f+16f+f+l16f+f+f+f+f+8f+8f+f+8.f+8f+f+f+8f+f+f+f+f+8a+f+8.a+>c+c+8c+8<bba+8a+8a+8.c+a+8f+8f+8f+d+f+8f+8d+c+8.a+>c+8O3A+O4d+8O3A+O4c+8O3A+O4d+8O3A+O4l8c+<ba+g+g+l16f+g+8f+4&f+c+f+f+f+8f+f+f+8f+4&f8&f+c+a+f+f+8f+d+d+8d+2a+8f+4f+d+f+8f+4f+d+f+8f+4f+4a+4.a+8f+4f+d+f+8f+4f+d+f+8l4f+f+a+.a+l8>c+.c+16<ba+g+g+f+2f+f+g+f+a+g+4.g+f+4g+a+d+2.l16f+f+f+8f+4f+f+f+f+f+8f+f+8c+f+f+f+8f+8f+8f+f+f+8a+f+c+c+a+>c+c+<b8a+8g+g+f+8.f+f+f+g+a+8g+f+g+f+d+8d+d+8c+a+8>c+8d+8.<c+a+f+f+f+f+8.c+f+g+a+O4C+O3b8a+g+a+g+d+8d+8.c+>d+8c+8c+c+8.<a+f+8f+4&f+f+f+f+g+a+f+f+g+a+f+d+8d+2a+8f+4f+d+f+8f+4f+d+f+8f+4f+4a+4.a+8f+4f+d+f+8f+4f+d+f+8l4f+f+a+.a+l8>c+.c+16<ba+g+g+f+2f+f+g+f+a+g+4.g+f+4g+a+l1d+&,L4O4V100l1.rrrrrrrrrrrr4l8o2d+.f16f+f+f+f+bbbb>cccc<bbbbf+f+f+f+bbbb>cccc<bbbbf+f+f+f+bbbb>cccc<bbbbf+f+f+f+eeeel1f+&f+&f+&f+&f+&f+&f+&f+&f+2.l8d+.f16f+f+f+f+bbbb>cccc<bbbbf+f+f+f+bbbb>cccc<bbbbf+f+f+f+bbbb>cccc<bbbbf+f+f+f+eeeef+&f+&f+&f+&f+&f+&f+&f+&f+")
(set! alls (map string (string->list alls)))

(define note? (curryr member '("a" "b" "c" "d" "e" "f" "g")))
(define num? (curryr member (map number->string '(0 1 2 3 4 5 6 7 8 9))))

(define evals (compose eval (curryr call-with-input-string read)))

(define (numify n)
  (if (equal? n "1") "" n))

(define (parsemml mml)
  #;(define num->note)
  (match mml
    ('() empty)
    (`("<" . ,rst) (cons < (parsemml rst)))
    (`(">" . ,rst) (cons > (parsemml rst)))
    (`(,note "+" ,number "." . ,rst)
     #:when (and (note? note) (num? number))
     (cons (evals (string-append note "#" (numify number) ".")) (parsemml rst)))
    (`(,note "+" ,number . ,rst)
     #:when (and (note? note) (num? number))
     (cons (evals (string-append note "#" (numify number))) (parsemml rst)))
    (`(,note ,number "." . ,rst)
     #:when (and (note? note) (num? number))
     (cons (evals (string-append note (numify number) ".")) (parsemml rst)))
    (`(,note ,number . ,rst)
     #:when (and (note? note) (num? number))
     (cons (evals (string-append note (numify number))) (parsemml rst)))
    (`(,note . ,rst)
     #:when (note? note)
     (cons (evals note) (parsemml rst)))
    (`(,char . ,rst)
     (displayln (format "Don't know char ~a" char))
     (parsemml rst))))

#;
(dump-to-midi
 "allstar.mid"
 (play))
