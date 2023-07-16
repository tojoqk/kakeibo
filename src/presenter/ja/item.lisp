(cl:in-package #:kakeibo/presenter/ja)

(coalton-toplevel
  (define-instance (presenter:Presenter JA presenter/itm:Term)
    (define (presenter:present (JA) trx)
      (match trx
        ((presenter/itm:Item) "項目")
        ((presenter/itm:Transaction) "取引")
        ((presenter/itm:Category) "カテゴリ")
        ((presenter/itm:Subcategory) "サブカテゴリ")
        ((presenter/itm:Amount) "金額")
        ((presenter/itm:Note) "メモ")))))
