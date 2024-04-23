#lang racket

(require "extra.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for extra functions of week 5"

   (check-equal?
    (take 6 nats) '(1 2 3 4 5 6) "test take function")
  
   (check-equal?
    (take 5 (until (lambda (x) (> x 5)) nats)) '(1 2 3 4 5) "test until function")

   (check-equal?
    (take 5 (map (lambda (x) (* x 2)) nats)) '(2 4 6 8 10) "test map function")

   (check-equal?
    (take 5 (zip nats powers)) '((1 . 2) (2 . 4) (3 . 8) (4 . 16) (5 . 32)) "test zip function")

   (check-equal?
    (take 5 (filter (lambda (x) (= (modulo x 3) 0)) nats)) '(3 6 9 12 15) "test filter function")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)