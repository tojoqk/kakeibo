(cl:defpackage #:kakeibo/global/date
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:integral #:coalton-library/math/integral)
   (#:exception #:kakeibo/global/exception))
  (:export
   #:Date
   #:make
   #:year #:month #:day

   #:Error
   #:InvalidDate))

(cl:in-package #:kakeibo/global/date)

(coalton-toplevel
  (define-type Date (%Date Integer      ; Year
                           Integer      ; Month
                           Integer      ; ;Date
                           ))

  (declare make (Integer
                 -> Integer
                 -> Integer
                 -> Result Error Date))
  (define (make y m d)
    (let date = (%Date y m d))
    (if (valid? date)
        (Ok date)
        (Err InvalidDate)))

  (define-type Error (InvalidDate))
  (exception:define-exception-instance Error)

  (define (year (%Date y _ _)) y)
  (define (month (%Date _ m _)) m)
  (define (day (%Date _ _ d)) d)

  (define-instance (Eq Date)
    (define (== (%Date y1 m1 d1) (%Date y2 m2 d2))
      (and (== y1 y2)
           (== m1 m2)
           (== d1 d2))))

  (define (leap? y)
    (and (== (integral:mod y 4) 0)
         (not (and (== (integral:mod y 100) 0)
                   (/= (integral:mod y 400) 0)))))

  (define (valid? (%Date y m d))
    (and (<= 0 y)
         (<= 1 d)
         (match m
           (1  (<= d 31))
           (2  (if (leap? y)
                   (<= d 29)
                   (<= d 28)))
           (3  (<= d 31))
           (4  (<= d 30))
           (5  (<= d 31))
           (6  (<= d 30))
           (7  (<= d 31))
           (8  (<= d 31))
           (9  (<= d 30))
           (10 (<= d 31))
           (11 (<= d 30))
           (12 (<= d 31))
           (_ False)))))
