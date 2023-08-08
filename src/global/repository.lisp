(cl:defpackage #:kakeibo/global/repository
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:export #:Repository))

(cl:in-package #:kakeibo/global/repository)

(coalton-toplevel
  (define-class (Repository :r)))
