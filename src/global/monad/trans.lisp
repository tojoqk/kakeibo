(cl:defpackage #:kakeibo/global/monad/trans
  (:use #:coalton
        #:coalton-prelude)
  (:export #:MonadTrans
           #:lift))

(cl:in-package #:kakeibo/global/monad/trans)

(coalton-toplevel
  (define-class (MonadTrans :t)
    (lift (Monad :m => :m :a -> :t :m :a))))
