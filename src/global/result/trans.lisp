(cl:defpackage #:kakeibo/global/result/trans
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:monad/trans #:kakeibo/global/monad/trans))
  (:export #:T #:run))

(cl:in-package #:kakeibo/global/result/trans)

(coalton-toplevel
  (define-type (T :a :m :b) (T (:m (Result :a :b))))

  (define (run (T m)) m)

  (define-instance (Functor :m => (Functor (T :a :m)))
    (define (map f (T m))
      (T (map (map f) m))))

  (define-instance (Applicative :m => (Applicative (T :a :m)))
    (define (pure x)
      (T (pure (pure x))))
    (define (liftA2 op (T m1) (T m2))
      (T (liftA2 (liftA2 op) m1 m2))))

  (define-instance (Monad :m => (Monad (T :a :m)))
    (define (>>= (T m) f)
      (T
       (>>= m
            (fn (m_)
              (match m_
                ((Ok x) (run (f x)))
                ((Err e) (pure (Err e)))))))))

  (define-instance (monad/trans:MonadTrans (T :a))
    (define (monad/trans:lift m)
      (T (>>= m (.< pure Ok))))))
