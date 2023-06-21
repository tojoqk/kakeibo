(cl:defpackage #:kakeibo/global/time
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:time
           #:error)
  (:local-nicknames
   (#:integral #:coalton-library/math/integral)
   (#:exception #:kakeibo/global/exception))
  (:export #:Time
           #:make

           #:Error #:InvalidTime

           #:year #:month #:day
           #:hours #:minutes #:seconds
           ))

(cl:in-package #:kakeibo/global/time)

(coalton-toplevel
  (define-type Time
    (%Time Integer))

  (define-type Error
    (InvalidTime Integer Integer Integer Integer Integer Integer))
  (exception:define-exception-instance Error)

  (declare make (Integer                ; year
                 -> Integer             ; month
                 -> Integer             ; day
                 -> Integer             ; hours
                 -> Integer             ; mintes
                 -> Integer             ; seconds
                 -> (Result Error Time)))
  (define (make y m d hs ms ss)
    (if (not (and (valid-date? y m d)
                  (<= 0 hs) (<= hs 23)
                  (<= 0 ms) (<= ms 59)
                  (<= 0 ss) (<= ss 59)))
        (Err (InvalidTime y m d hs ms ss))
        (Ok (%Time
             (lisp Integer (y m d hs ms ss)
               (cl:encode-universal-time
                ss
                ms
                hs
                d
                m
                y))))))

  (define (get-time!)
    (%Time (lisp Integer () (cl:get-universal-time))))

  (declare seconds (Time -> Integer))
  (define (seconds (%Time tm))
    (lisp Integer (tm)
      (cl:multiple-value-bind (secs)
          (cl:decode-universal-time tm)
        secs)))

  (declare minutes (Time -> Integer))
  (define (minutes (%Time tm))
    (lisp Integer (tm)
      (cl:multiple-value-bind (secs mins)
          (cl:decode-universal-time tm)
        (cl:declare (cl:ignore secs))
        mins)))

  (declare hours (Time -> Integer))
  (define (hours (%Time tm))
    (lisp Integer (tm)
      (cl:multiple-value-bind (secs mins hours)
          (cl:decode-universal-time tm)
        (cl:declare (cl:ignore secs mins))
        hours)))

  (declare day (Time -> Integer))
  (define (day (%Time tm))
    (lisp Integer (tm)
      (cl:multiple-value-bind (secs mins hours d)
          (cl:decode-universal-time tm)
        (cl:declare (cl:ignore secs mins hours))
        d)))

  (declare month (Time -> Integer))
  (define (month (%Time tm))
    (lisp Integer (tm)
      (cl:multiple-value-bind (secs mins hours d m)
          (cl:decode-universal-time tm)
        (cl:declare (cl:ignore secs mins hours d))
        m)))

  (declare year (Time -> Integer))
  (define (year (%Time tm))
    (lisp Integer (tm)
      (cl:multiple-value-bind (secs mins hours d m y)
          (cl:decode-universal-time tm)
        (cl:declare (cl:ignore secs mins hours d m))
        y)))

  (define (leap? y)
    (and (== (integral:mod y 4) 0)
         (not (and (== (integral:mod y 100) 0)
                   (/= (integral:mod y 400) 0)))))

  (define (valid-date? y m d)
    (and (<= 1900 y)
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
