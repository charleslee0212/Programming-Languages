#lang racket

;;squareSum
;;The function squares all the number from 0 to n and adds them up
;;The parameter is an integer greater than zero
;;Returns the sum of squared numbers 
(define (squareSum n)
  (squareSumHelper n 0))

;;squareSumHelper
;;Helper function for squareSum
(define (squareSumHelper n a)
  (if (> n 0)
      (squareSumHelper (- n 1) (+ (* n n) a))
      a))

;;cycleOne
;;The function cycles the first element in the list to the back of the list
;;The parameter is a list
;;Returns a list
(define (cycleOne l)
  (append (cdr l) (list (car l))))

;;cycleN
;;The function cycles the elements in a list c number of times
;;The parameters are a count number and a list
;;Returns a list
(define (cycleN c l)
  (if (> c 0)
      (cycleN (- c 1) (append (cdr l) (list (car l))))
      l))

;;memberOf
;;The function checks to see if the given variable is within the given list
;;The parameters are a value and a list
;;Returns a boolean
(define (memberOf v l)
  (if (empty? l)
      #f
      (if (equal? v (car l))
          #t
          (memberOf v (cdr l)))))

;;intersection
;;The function intersects two list
;;The parameter are two lists
;;Returns a list of intersection
(define (intersection l l*)
  (if (empty? l)
      null
      (if (memberOf (car l) l*)
          (append (list (car l)) (intersection (cdr l) l*))
          (intersection (cdr l) l*))))

  