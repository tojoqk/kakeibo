(defpackage #:kakeibo/presenter/language
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Language))

(cl:in-package #:kakeibo/presenter/language)

(coalton-toplevel
  (define-class (Language :lang)))
