#lang racket

(provide (all-defined-out))

;; Provided structures


;; Delays
(define (my-delay th) 
  (mcons #f th)) ;; a one-of "type" we will update /in place/

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

;; Streams
(define (mk-stream fn arg)
  (letrec ([f (lambda (x) 
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
(define nats  (mk-stream + 1))
(define powers (mk-stream * 2))
;; End ...

(define (take n st)
  (if (= 0 n)
      null
      (let ([cur (st)])
        (cons (car cur) (take (- n 1) (cdr cur))))))

(define (until fun st)
  (letrec ([f (lambda (st)
                (let ([cur (st)])
                  (if (not (fun (car cur)))
                      (cons (car cur) (until fun (cdr cur)))
                      null)))])
    (lambda () (f st))))

(define (map fun st)
  (letrec ([f (lambda (st)
                (let ([cur (st)])
                      (cons (fun (car cur)) (map fun (cdr cur)))
                      ))])
    (lambda () (f st))))

(define (zip s1 s2)
  (letrec ([f (lambda (st1 st2)
                (let ([cur1 (st1)]
                      [cur2 (st2)])
                  
                      (cons (cons (car cur1) (car cur2)) (zip (cdr cur1) (cdr cur2)))
                      ))])
    (lambda () (f s1 s2))))

(define (filter pred st)
  (letrec ([f (lambda (st)
                (let ([cur (st)])
                  (if (pred (car cur))
                      (cons (car cur) (filter pred (cdr cur)))
                      ((filter pred (cdr cur)))
                      )))])
    (lambda () (f st))))