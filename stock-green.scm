(import chicken scheme)
(use numbers)

(define (random-percent)
  (* 0.000001 (random 1000000)))

(define (random-pos-or-neg)
  (expt -1 (random 100)))

(define (change-pct rnd pos-or-neg skew)
  (let ((rnd-with-sign (* rnd pos-or-neg)))
    (+ rnd-with-sign (* (- 1 (abs rnd-with-sign)) skew))))

(define (next-value current-value max-change skew)
  (+ current-value (* current-value (change-pct (random-percent) (random-pos-or-neg) skew))))

(next-value 100 1 -1)
(change-pct 1 1 -1)
