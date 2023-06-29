(defpackage #:kakeibo/global/result
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/result)
  (:export #:ok?
           #:err?
           #:map-err
           #:flatten

           #:from-optional))

(cl:in-package #:kakeibo/global/result)

(coalton-toplevel
  (define (from-optional e opt)
    (match opt
      ((Some x) (Ok x))
      ((None) (Err e)))))
