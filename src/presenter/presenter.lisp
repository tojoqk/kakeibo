(defpackage #:kakeibo/presenter/presenter
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:lang #:kakeibo/presenter/language))
  (:export  #:Presenter
            #:present))

(cl:in-package #:kakeibo/presenter/presenter)

(coalton-toplevel
  (define-class (lang:Language :lang => Presenter :lang :a)
    (present (:lang -> :a -> String))))
