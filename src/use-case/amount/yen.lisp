(cl:defpackage #:kakeibo/use-case/amount/yen
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:type #:kakeibo/entity/type)
   (#:amount #:kakeibo/use-case/amount))
  (:export #:Yen))

(cl:in-package #:kakeibo/use-case/amount/yen)

(coalton-toplevel
  (define-type Yen (Yen Integer))

  (define-instance (Eq Yen)
    (define (== (Yen x) (Yen y)) (== x y)))

  (define-instance (Ord Yen)
    (define (<=> (Yen x) (Yen y)) (<=> x y)))

  (define-instance (Semigroup Yen)
    (define (<> (Yen x) (Yen y)) (Yen (+ x y))))

  (define-instance (Monoid Yen)
    (define mempty (Yen 0)))

  (define-instance (amount:Amount Yen)
    (define (amount:amount type n)
      (match type
        ((type:Income) (Yen n))
        ((type:Outgo) (Yen (negate n)))))))
