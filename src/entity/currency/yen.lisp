(cl:defpackage #:kakeibo/entity/currency/yen
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:currency #:kakeibo/entity/currency))
  (:export #:Yen))

(cl:in-package #:kakeibo/entity/currency/yen)

(coalton-toplevel
  (define-type Yen (Yen Integer))

  (define-instance (Eq Yen)
    (define (== (Yen x) (Yen y)) (== x y)))

  (define-instance (Ord Yen)
    (define (<=> (Yen x) (Yen y)) (<=> x y)))

  (define-instance (Num Yen)
    (define (+ (Yen x) (Yen y)) (Yen (+ x y)))
    (define (- (Yen x) (Yen y)) (Yen (- x y)))
    (define (* (Yen x) (Yen y)) (Yen (* x y)))
    (define (fromInt x) (Yen x)))

  (define-instance (currency:Currency Yen)
    (define (currency:make x) (Yen x))))
