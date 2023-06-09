(cl:defpackage #:kakeibo/global/transformer/monad
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes)
  (:export #:MonadTrans
           #:lift))

(cl:in-package #:kakeibo/global/transformer/monad)

(coalton-toplevel
  (define-class (MonadTrans :t)
    (lift (Monad :m => :m :a -> :t :m :a))))
