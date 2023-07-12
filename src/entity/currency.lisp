(cl:defpackage #:kakeibo/entity/currency
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Currency #:make))

(cl:in-package #:kakeibo/entity/currency)

(coalton-toplevel
  (define-class ((Ord :a) (Num :a) => Currency :a)
    (make (Integer -> :a))))
