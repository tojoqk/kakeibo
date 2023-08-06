(cl:defpackage #:kakeibo/global/repository
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:export #:Repository
           #:Error))

(cl:in-package #:kakeibo/global/repository)

(coalton-toplevel
  (define-class (Repository :r))
  (define-class (Error :e)))
