(cl:defpackage #:kakeibo/global/repository
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Repository #:connected?
           #:Config #:connect!))

(cl:in-package #:kakeibo/global/repository)

(coalton-toplevel
  (define-class (Repository :r)
    (connected? (:r -> Boolean)))

  (define-class (Repository :r => Config :r :c (:r -> :c))
    (connect! (:c -> :r))))
