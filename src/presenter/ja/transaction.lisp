(cl:in-package #:kakeibo/presenter/ja)

(coalton-toplevel
  (define-instance (presenter:Presenter JA presenter/trx:Term)
    (define (presenter:present (JA) trx)
      (match trx
        ((presenter/trx:Transaction) "取引")
        ((presenter/trx:Date) "日付")
        ((presenter/trx:Type) "種別")
        ((presenter/trx:Note) "メモ")
        ((presenter/trx:Amount) "金額")))))
