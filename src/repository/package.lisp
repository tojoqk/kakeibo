(defpackage #:kakeibo/repository/ram
  (:use #:coalton
        #:coalton-prelude
        #:kakeibo/global/identity
        #:kakeibo/global/transformer/result
        #:kakeibo/global/transformer/monad)
  (:local-nicknames
   (#:st #:coalton-library/monad/state)
   (#:item #:kakeibo/entity/item)
   (#:transaction #:kakeibo/entity/transaction)
   (#:valid #:kakeibo/global/valid)
   (#:iter #:coalton-library/iterator)
   (#:map #:coalton-library/ord-map)))

