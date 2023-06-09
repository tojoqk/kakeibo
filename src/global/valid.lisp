(cl:defpackage #:kakeibo/global/valid
  (:use #:coalton
        #:coalton-library/classes)
  (:local-nicknames
   (#:result #:coalton-library/result))
  (:export
   Valid
   get
   Validatable
   validate
   valid?

   ValidatableM
   validateM
   validM?
   validM))

(cl:in-package #:kakeibo/global/valid)

(coalton-toplevel
  (define-type (Valid :a) (%Valid :a))
  (define (get (%Valid x)) x)

  (define-instance (Eq :a => Eq (Valid :a))
    (define (== (%Valid x) (%Valid y))
      (== x y)))

  (define-class (Validatable :a :b (:a -> :b))
    (validate (:a -> (Result :b Unit))))

  (declare valid (Validatable :a :b => :a -> (Result :b (Valid :a))))
  (define (valid x)
    (>>= (validate x)
         (fn ((Unit))
           (pure (%Valid x)))))

  (define (valid? x)
    (match (validate x)
      ((Ok (Unit)) True)
      ((Err _) False)))

  (define-class (ValidatableM :m :a :b (:a -> :b))
    (validateM (:a -> :m (Result :b Unit))))

  (declare validM ((Monad :m) (ValidatableM :m :a :b) => :a -> :m (Result :b (Valid :a))))
  (define (validM x)
    (>>= (validateM x)
         (fn (result)
           (pure
            (>>= result
                 (fn ((Unit))
                   (pure (%Valid x))))))))

  (declare validM? ((Monad :m) (ValidatableM :m :a :b) => :a -> :m Boolean))
  (define (validM? x)
    (map result:ok? (validateM x)))
  )
