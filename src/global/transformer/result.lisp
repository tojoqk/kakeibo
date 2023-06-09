(cl:defpackage #:kakeibo/global/transformer/result
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes
        #:kakeibo/global/transformer/monad)
  (:export #:ResultT
           #:runResultT))

(cl:in-package #:kakeibo/global/transformer/result)

(coalton-toplevel
  (define-type (ResultT :a :m :b) (ResultT (:m (Result :a :b))))

  (define (runResultT (ResultT m)) m)

  (define-instance (Functor :m => (Functor (ResultT :a :m)))
    (define (map f (ResultT m))
      (ResultT (map (map f) m))))

  (define-instance (Applicative :m => (Applicative (ResultT :a :m)))
    (define (pure x)
      (ResultT (pure (pure x))))
    (define (liftA2 op (ResultT m1) (ResultT m2))
      (ResultT (liftA2 (liftA2 op) m1 m2))))

  (define-instance (Monad :m => (Monad (ResultT :a :m)))
    (define (>>= (ResultT m) f)
      (ResultT
       (>>= m
            (fn (m_)
              (match m_
                ((Ok x) (runResultT (f x)))
                ((Err e) (pure (Err e)))))))))

  (define-instance (MonadTrans (ResultT :a))
    (define (lift m)
      (ResultT (>>= m (fn (x) (pure (Ok x))))))))
