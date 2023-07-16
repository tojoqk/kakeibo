(cl:in-package #:kakeibo/presenter/ja)

(coalton-toplevel
  (define-instance (presenter:Presenter JA type:Type)
    (define (presenter:present (JA) type)
      (match type
        ((type:Income) "入金")
        ((type:Outgo) "出金")))))
