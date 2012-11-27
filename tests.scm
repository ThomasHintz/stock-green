(import chicken scheme)
(use test)

(load "stock-green.scm")

(define (make-test-stock)
  (make-stock name: "test" price: 1000.432432 projected: 0.05123 last: -0.012
              avg: 0.022 recent-avg: 0.034 volatility: 0.1 value: 100))

(test-begin)
(test-group
 "store?"
 (test #f (store? '()))
 (test #t (store? '(a)))
 (test-error (store? 1)))

(test-group
 "accessor"
 (accessor stock-name 'name)
 (test "test" (stock-name (make-test-stock)))
 (test "food" (stock-name (stock-name (make-test-stock) "food")))
 (test-error (stock-name 2)))

(test-group
 "combine percents"
 (test 16 (combine-percents '((2 2) (3 4))))
 (test 0 (combine-percents '()))
 (test 4 (combine-percents '((2 2)))))

(test-group
 "stock values"
 (test 102.56 (projected-value (make-test-stock)))
 (test 102.34 (generate-value (make-test-stock) 0 0)))

(test-group
 "utilities"
 (test 50.5 (->pct 0.505))
 (test "50.50%" (->pct-string 0.505))
 (test "$500,000.08" (->$ 500000.08)))

(test-end)
