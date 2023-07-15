(cl:defpackage #:kakeibo/entity/type
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Type
           #:Income #:Outgo))

(cl:in-package #:kakeibo/entity/type)

(coalton-toplevel
  (define-type Type
    (Income)
    (Outgo))

  (define-instance (Eq Type)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Income) (Income)) True)
        ((Tuple (Outgo) (Outgo)) True)
        (_ False)))))
