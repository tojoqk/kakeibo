(cl:defpackage #:kakeibo/presenter/item
  (:use #:coalton
        #:coalton-prelude)
  (:export
   #:Term
   #:Item
   #:Transaction
   #:Category
   #:Subcategory
   #:Amount
   #:Note))

(cl:in-package #:kakeibo/presenter/item)

(coalton-toplevel
  (define-type Term
    Item
    Transaction
    Category
    Subcategory
    Amount
    Note))

