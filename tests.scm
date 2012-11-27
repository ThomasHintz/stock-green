(import chicken scheme)
(use test)

(load "stock-green.scm")

(define (make-test-stock)
  (make-stock name: "test" price: 1000.432432 projected: 0.05123 last: -0.012
              avg: '((samples . 2) (value . 2.0)) recent-avg: 0.034 volatility: 0.1 value: 20))

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
 "cumulative avg"
 (test 20 (cumulative-avg 2 30 15))
 (test 10 (cumulative-avg 0 10 0))
 (test 2 (stock-avg-samples (make-test-stock)))
 (test 2.0 (stock-avg-value (make-test-stock)))
 (test 4 (stock-avg-samples (stock-avg-samples (make-test-stock) 4)))
 (test 4 (stock-avg-value (stock-avg-value (make-test-stock) 4)))
 (test "183.33%" (->pct-string (stock-cumulative-avg (make-test-stock) 30))))

(test-group
 "stock values"
 (test 34.204 (projected-value (make-test-stock)))
 (test 32.204 (generate-value (make-test-stock) 0 0)))

(test-group
 "utilities"
 (test 50.5 (->pct 0.505))
 (test "50.50%" (->pct-string 0.505))
 (test "$500,000.08" (->$ 500000.08)))

(test-end)
