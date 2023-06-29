(defpackage #:kakeibo/global/result
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/result)
  (:local-nicknames
   (#:e #:kakeibo/global/exception))
  (:export #:ok?
           #:err?
           #:map-err
           #:flatten

           #:OptionalError
           #:from-optional))

(cl:in-package #:kakeibo/global/result)

(coalton-toplevel
  (define-type OptionalError OptionalError)
  (e:define-exception-instance OptionalError)

  (define (from-optional opt)
    (match opt
      ((Some x) (Ok x))
      ((None) (Err OptionalError)))))
