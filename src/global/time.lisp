(cl:defpackage #:kakeibo/global/time
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:time
           #:error)
  (:local-nicknames
   (#:integral #:coalton-library/math/integral)
   (#:result #:coalton-library/result)
   (#:list #:coalton-library/list)
   (#:exception #:kakeibo/global/exception)
   (#:date #:kakeibo/global/date))
  (:export #:Time
           #:make
           #:get!

           #:Error #:InvalidTime

           #:year #:month #:day
           #:hours #:minutes #:seconds
           ))

(cl:in-package #:kakeibo/global/time)

(coalton-toplevel
  (define-type Time
    (%Time Integer))

  (define-instance (Eq Time)
    (define (== (%Time x) (%Time y))
      (== x y)))

  (define-instance (Ord Time)
    (define (<=> (%Time x) (%Time y))
      (<=> x y)))

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
    (if (and (result:ok? (date:make y m d))
             (<= 0 hs) (<= hs 23)
             (<= 0 ms) (<= ms 59)
             (<= 0 ss) (<= ss 59))
        (Ok (%Time
             (lisp Integer (y m d hs ms ss)
               (cl:encode-universal-time
                ss
                ms
                hs
                d
                m
                y))))
        (Err (InvalidTime y m d hs ms ss))))

  (define (get!)
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

  (declare date (Time -> date:Date))
  (define (date t)
    (expect "(time:date) Unexpected Error"
            (date:make (year t)
                       (month t)
                       (day t))))

  (define-instance (Into date:Date Time)
    (define (into d)
      (expect "(time:into/date/time) Unexpected Error"
              (make (date:year d)
                    (date:month d)
                    (date:day d)
                    0
                    0
                    0)))))
