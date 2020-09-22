#lang racket

;;Test functions
(define (streammaker func arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (func x arg)))))])
    (lambda () (f arg))))

(define ones* (streammaker (lambda (a b) 1) 1))
(define nats* (streammaker + 1))
(define powersoftwo* (streammaker * 2))

;;next-k-items
;;The function traverses the stream and builds a list until k
;;The parameters are a stream and an integer k
;;Returns a list
(define (next-k-items s k)
  (if (> k 1)
      (append (list (car (s))) (next-k-items (cdr (s)) (- k 1)))
      (list (car (s)))))

;;kth-item
;;The function traverses the stream and finds the kth element
;;The parameters are a stream and an integer k
;;Returns an element
(define (kth-item s k)
  (if (> k 1)
      (kth-item (cdr (s)) (- k 1))
      (car (s))))