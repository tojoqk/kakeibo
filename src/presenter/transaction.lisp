(cl:defpackage #:kakeibo/presenter/transaction
  (:use #:coalton
        #:coalton-prelude)
  (:export
   #:Term
   #:Transaction
   #:Date
   #:Type
   #:Note
   #:Amount))

(cl:in-package #:kakeibo/presenter/transaction)

(coalton-toplevel
  (define-type Term
    Transaction
    Date
    Type
    Note
    Amount))
