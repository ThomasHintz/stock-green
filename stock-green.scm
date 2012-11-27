(import chicken scheme data-structures)
(use numbers fox srfi-1)

;;;;;;;;;;;;;;;; generic ;;;;;;;;;;;;;;
(define (store? test)
  (assert (list? test))
  (not (null? test)))

(define-syntax mapm
  (syntax-rules ()
    ((_ mac ls)
     (let ((mac-list (map (lambda (lst) (cons 'mac lst)) ls)))
       (eval
        `(begin
           ,@mac-list)
        (interaction-environment))))))

(define-syntax accessor
  (syntax-rules ()
    ((_ name key)
     (define (name alist . val)
       (assert (list? alist))
       (if (store? val)
           (alist-update key (car val) alist)
           (alist-ref key alist))))))

(define (combine-percents l)
  (fold + 0 (map (lambda (e) (* (car e) (cadr e))) l)))

;;;;;;;;;;;;;;;; equations ;;;;;;;;;;;;;
(define (random-percent)
  (* 0.000001 (random 1000000)))

(define (random-pos-or-neg)
  (expt -1 (random 100)))

(define (projected-value stock)
  (+ (stock-value stock)
     (* (combine-percents
         `((,(stock-avg stock) 0.7)
           (,(stock-recent-avg stock) 0.3)))
        (stock-value stock))))

(define (volatile-change stock-value volatility)
  (if (< (random-percent) volatility)
      (* stock-value (* (random-percent) (random-pos-or-neg)))
      0))

(define (generate-value stock random-change volatility-change)
  (+ (stock-value stock)
     (* (combine-percents
         `((,(stock-avg stock) 0.6)
           (,(stock-recent-avg stock) 0.3)
           (,random-change 0.1)))
        (stock-value stock))
     volatility-change))

;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;
(define (->pct num) (* 100 num))
(define (->pct-string num)
  (fox (->pct num) 0 #f '(2) "%"))
(define (->$ num)
  (string-append "$"
                 (fox num '(2) '#("," 3))))

;;;;;;;;;;;;;;; stocks ;;;;;;;;;;;;;;;;
(define (make-stock #!key (name "") (price 0) (projected 0) (last 0)
                    (avg 0) (recent-avg 0) (volatility 'none)
                    (value 0))
  `((name . ,name) (price . ,price) (projected . ,projected)
    (last . ,last) (avg . ,avg)
    (recent-avg . ,recent-avg) (volatility . ,volatility)
    (value . ,value)))

(mapm accessor
      '((stock-name 'name)
        (stock-price 'price)
        (stock-projected 'projected)
        (stock-last 'last)
        (stock-avg 'avg)
        (stock-recent-avg 'recent-avg)
        (stock-volatility 'volatility)
        (stock-value 'value)))

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
    (print (stock-volatility stock))
    (fox "value: " padding #t)
    (print (stock-value stock))))

(define (make-test-stock)
  (make-stock name: "test" price: 1000.432432 projected: 0.05123 last: -0.012
              avg: 0.022 recent-avg: 0.034 volatility: 0.1 value: 100))
