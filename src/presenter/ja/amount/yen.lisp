(cl:in-package #:kakeibo/presenter/ja)

(coalton-toplevel
  (define-instance (presenter:Presenter JA yen:Yen)
    (define (presenter:present (JA) (yen:Yen n))
      (<> (into n) "円"))))
