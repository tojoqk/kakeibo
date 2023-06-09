(cl:defpackage #:kakeibo/global/identity
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes)
  (:export #:Identity
           #:runIdentity))

(cl:in-package #:kakeibo/global/identity)

(coalton-toplevel
  (define-type (Identity :a) (Identity :a))

  (define (runIdentity (Identity x)) x)

  (define-instance (Functor Identity)
    (define (map f (Identity x))
      (Identity (f x))))

  (define-instance (Applicative Identity)
    (define pure Identity)
    (define (liftA2 f (Identity x) (Identity y))
      (Identity (f x y))))

  (define-instance (Monad Identity)
    (define (>>= (Identity x) f) (f x)))

  (define-instance (Eq :a => Eq (Identity :a))
    (define (== (Identity x) (Identity y)) (== x y)))

  (define-instance (Ord :a => Ord (Identity :a))
    (define (<=> (Identity x) (Identity y)) (<=> x y)))
  )
