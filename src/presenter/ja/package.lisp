(cl:defpackage #:kakeibo/presenter/ja
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:presenter #:kakeibo/presenter/presenter)
   (#:lang #:kakeibo/presenter/language)
   (#:type #:kakeibo/entity/type)
   (#:yen #:kakeibo/use-case/amount/yen)
   (#:presenter/trx #:kakeibo/presenter/transaction)
   (#:presenter/itm #:kakeibo/presenter/item)
   (#:entity/trx #:kakeibo/entity/transaction)
   (#:entity/itm #:kakeibo/entity/transaction))
  (:export #:JA))
