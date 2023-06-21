(cl:defpackage #:kakeibo/global/date
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:integral #:coalton-library/math/integral)
   (#:exception #:kakeibo/global/exception)
   (#:time #:kakeibo/global/time))
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
                           Integer      ; Day
                           ))

  (declare make (Integer
                 -> Integer
                 -> Integer
                 -> Result Error Date))
  (define (make y m d)
    (let date = (%Date y m d))
    (match (time:make y m d 0 0 0)
      ((Ok _) (Ok date))
      ((Err _) (Err InvalidDate))))

  (define-type Error (InvalidDate))
  (exception:define-exception-instance Error)

  (define (year (%Date y _ _)) y)
  (define (month (%Date _ m _)) m)
  (define (day (%Date _ _ d)) d)

  (define-instance (Eq Date)
    (define (== (%Date y1 m1 d1) (%Date y2 m2 d2))
      (and (== y1 y2)
           (== m1 m2)
           (== d1 d2)))))
