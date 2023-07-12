(defpackage #:kakeibo/global/presenter
  (:use #:coalton
        #:coalton-prelude)
  (:export #:Language
           #:Presenter
           #:present))

(cl:in-package #:kakeibo/global/presenter)

(coalton-toplevel
  (define-class (Language :lang))

  (define-class (Language :lang => Presenter :lang :a)
    (present (:lang -> :a -> String))))
