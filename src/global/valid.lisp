(cl:defpackage #:kakeibo/global/valid
  (:use #:coalton
        #:coalton-library/classes
        #:kakeibo/global/transformer/result)
  (:local-nicknames
   (#:result #:coalton-library/result))
  (:export
   Valid
   get
   Validatable
   validate))

(cl:in-package #:kakeibo/global/valid)

(coalton-toplevel
  (define-type (Valid :a) (%Valid :a))
  (define (get (%Valid x)) x)

  (define-instance (Eq :a => Eq (Valid :a))
    (define (== (%Valid x) (%Valid y))
      (== x y)))

  (define-class (Validatable :m :a :b (:a -> :b))
    (validate (:a -> (ResultT :b :m Unit))))

  (declare valid ((Monad :m) (Validatable :m :a :b) => :a -> (ResultT :b :m (Valid :a))))
  (define (valid x)
    (>>= (validate x)
         (fn ((Unit))
           (pure (%Valid x))))))
