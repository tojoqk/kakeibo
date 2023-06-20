(cl:defpackage #:kakeibo/global/result/trans
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:some)
  (:local-nicknames
   (#:monad/trans #:kakeibo/global/monad/trans)
   (#:result #:coalton-library/result)
   (#:exception #:kakeibo/global/exception))
  (:export #:ResultT #:run
           #:some
           #:flat #:flat*))

(cl:in-package #:kakeibo/global/result/trans)

(coalton-toplevel
  (define-type (ResultT :a :m :b) (ResultT (:m (Result :a :b))))

  (define (run (ResultT m)) m)

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
                ((Ok x) (run (f x)))
                ((Err e) (pure (Err e)))))))))

  (define-instance (monad/trans:MonadTrans (ResultT :a))
    (define (monad/trans:lift m)
      (ResultT (>>= m (.< pure Ok)))))

  (declare some ((Monad :m) (exception:Exception :e) =>
                 (ResultT :e :m :a) ->
                 (ResultT exception:Some :m :a)))
  (define (some m)
    (ResultT (map (result:map-err exception:to) (run m))))

  (define (join mm)
    (do (m <- mm)
        m))

  (declare flat ((Monad :m) =>
                 ResultT :e (ResultT :e :m) :a ->
                 ResultT :e :m :a))
  (define (flat m)
    (ResultT (map join (run (run m)))))

  (declare flat* ((Monad :m)
                  (exception:Exception :e1)
                  (exception:Exception :e2) =>
                  ResultT :e1 (ResultT :e2 :m) :a ->
                  ResultT exception:Some :m :a))
  (define (flat* m)
    (ResultT (map join (run (some (run (some m))))))))
