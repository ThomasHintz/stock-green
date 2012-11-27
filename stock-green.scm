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
(define (cumulative-avg k new-value average)
  (/ (+ new-value (* k average)) (+ k 1)))

(define (random-percent)
  (* 0.000001 (random 1000000)))

(define (random-pos-or-neg)
  (expt -1 (random 100)))

(define (projected-value stock)
  (+ (stock-value stock)
     (* (combine-percents
         `((,(- (stock-avg-value stock) 1) 0.7)
           (,(stock-recent-avg stock) 0.3)))
        (stock-value stock))))

(define (volatile-change stock-value volatility)
  (if (< (random-percent) volatility)
      (* stock-value (* (random-percent) (random-pos-or-neg)))
      0))

(define (generate-value stock random-change volatility-change)
  (+ (stock-value stock)
     (* (combine-percents
         `((,(- (stock-avg-value stock) 1) 0.6)
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
                    (avg '((samples . 0) (value . 0))) (recent-avg 0) (volatility 0)
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
        (stock-samples 'samples)
        (stock-recent-avg 'recent-avg)
        (stock-volatility 'volatility)
        (stock-value 'value)))

(define (stock-avg-samples stock . val)
  (if (store? val)
      (stock-avg stock (stock-samples (stock-avg stock) (car val)))
      (stock-samples (stock-avg stock))))

(define (stock-avg-value stock . val)
  (if (store? val)
      (stock-avg stock (stock-value (stock-avg stock) (car val)))
      (stock-value (stock-avg stock))))

(define (stock-cumulative-avg stock new-value)
  (cumulative-avg (stock-avg-samples stock)
                  (/ new-value (stock-value stock))
                  (stock-avg-value stock)))

(define (display-stock stock)
  (let ((padding 18))
    (print "")
    (fox (stock-name stock) (+ padding (string-length (stock-name stock))) #t "\n")
    (fox "price: " padding #t)
    (fox (->$ (stock-price stock)) 0 #t '(2) "\n")
    (fox "projected: " padding #t)
    (fox (->pct-string (stock-projected stock)) #t "\n")
    (fox "last: " padding #t)
    (fox (->pct-string (stock-last stock)) #t "\n")
    (fox "average: " padding #t)
    (fox (->pct-string (- (stock-avg-value stock) 1)) #t "\n")
    (fox "recent average: " padding #t)
    (fox (->pct-string (stock-recent-avg stock)) #t "\n")
    (fox "volatility: " padding #t)
    (fox (->pct-string (stock-volatility stock)) #t "\n")
    (fox "value: " padding #t)
    (print (stock-value stock))))

(define (make-test-stock)
  (make-stock name: "test" price: 1000.432432 projected: 0.05123 last: -0.012
              avg: '((samples . 0) (value . 0)) recent-avg: 0.034 volatility: 0.1 value: 30))

;;;;;;;;;;;;; game play ;;;;;;;;;;;;;;;;
(define (update-stock stock new-value)
  (let ((stock (list-copy stock)))
    (set! stock (stock-avg-value stock (stock-cumulative-avg stock new-value)))
    (set! stock (stock-avg-samples stock (+ 1 (stock-avg-samples stock))))
    (set! stock (stock-value stock new-value))
    stock))
;; (stock-avg-value (make-test-stock) 1)
;; (stock-avg (make-test-stock))
(let ((stock (make-test-stock)))
  (set! stock (update-stock stock 30))
  (set! stock (update-stock stock (generate-value stock
                                                  (* (random-percent) (random-pos-or-neg))
                                                  (volatile-change (stock-value stock) (stock-volatility stock)))))
  (display-stock stock))

;; (define google
;;   (make-stock name: "Google" price: 100 volatility: 0.01 value: 100))
;; (set! google
;; (stock-value google (generate-value google
;;                                     (* (random-percent) (random-pos-or-neg))
;;                                     (volatile-change (stock-value google) (stock-volatility google)))))
;; (display-stock google)
