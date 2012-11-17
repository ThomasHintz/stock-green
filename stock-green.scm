(import chicken scheme data-structures)
(use numbers fox)

;;;;;;;;;;;;;;;; generic ;;;;;;;;;;;;;;
(define (store? test)
  (not (null? test)))

(define-syntax accessor
  (syntax-rules ()
    ((_ name key)
     (define (name stock . val)
       (if (store? val)
           (alist-update key (car val) stock)
           (alist-ref key stock))))))

;;;;;;;;;;;;;;;; equations ;;;;;;;;;;;;;
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

;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;
(define (->pct num) (* 100 num))
(define (->pct-string num)
  (fox (->pct num) 0 #f '(2) "%"))
(define (->$ num)
  (string-append "$"
                 (fox num '(2) '#("," 3))))

;;;;;;;;;;;;;;; stocks ;;;;;;;;;;;;;;;;

(define (make-stock #!key (name "") (price 0) (projected 0) (last 0)
                    (avg 0) (recent-avg 0) (volatility 'none))
  `((name . ,name) (price . ,price) (projected . ,projected)
    (last . ,last) (avg . ,avg)
    (recent-avg . ,recent-avg) (volatility . ,volatility)))

(accessor stock-name 'name)
(accessor stock-price 'price)
(accessor stock-projected 'projected)
(accessor stock-last 'last)
(accessor stock-avg 'avg)
(accessor stock-recent-avg 'recent-avg)
(accessor stock-volatility 'volatility)

(define (display-stock stock)
  (let ((padding 18))
    (print "")
    (fox (stock-name stock) (+ padding 4) #t "\n")
    (fox "price: " padding #t)
    (fox (->$ (stock-price stock)) 0 #t '(2) "\n")
    (fox "projected: " padding #t)
    (fox (->pct-string (stock-projected stock)) #t "\n")
    (fox "last: " padding #t)
    (fox (->pct-string (stock-last stock)) #t "\n")
    (fox "average: " padding #t)
    (fox (->pct-string (stock-avg stock)) #t "\n")
    (fox "recent average: " padding #t)
    (fox (->pct-string (stock-recent-avg stock)) #t "\n")
    (fox "volatility: " padding #t)
    (print (stock-volatility stock))))

(define (make-test-stock)
  (make-stock name: "test" price: 1000.432432 projected: 0.05123 last: -0.012
              avg: 0.022 recent-avg: 0.034 volatility: 'medium))
(display-stock (make-test-stock))

(fox (->pct 0.142212312) '(01))
