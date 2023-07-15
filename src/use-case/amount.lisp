(cl:defpackage #:kakeibo/use-case/amount
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Amount)
  (:local-nicknames
   (#:type #:kakeibo/entity/type)))

(cl:in-package #:kakeibo/use-case/amount)

(coalton-toplevel
  (define-class ((Ord :a) (Monoid :a) => Amount :a)
    (amount (type:Type -> Integer -> :a))))
