#lang racket

;;allTrue
;;This function searches through a list of booleans and checks to see if all is true
;;The paramter is a list of boolean
;;Returns a boolean
(define (allTrue list)
  (if (empty? list)
      #t
      (if (car list)
          (allTrue (rest list))
          #f)))

;;countIncreases
;;This functionn searches through a list of int and checks to see any increases between two ints
;;The parameter is a list of ints
;;Returns an int of increases
(define (countIncreases list)
  (countIncreasesHelper list 0))

;;countIncreasesHelper
;;helper function for countIncreases
(define (countIncreasesHelper list count)
    (if (empty? list)
        count
        (if (> (length list) 1)
            (if (< (car list) (cadr list))
                (countIncreasesHelper (rest list) (+ count 1))
                (countIncreasesHelper (rest list) count))
            count)))

;;downSeries
;;The function steps down from a high int to the low int depending on the step
;;The parameters are step high low
;;Returns a list of ints
(define (downSeries step high low)
  (if (< high low)
      null
      (cons high (downSeries step (- high step) low)))) 
  