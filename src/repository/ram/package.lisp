(defpackage #:kakeibo/repository/ram
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:st #:coalton-library/monad/state)
   (#:item #:kakeibo/entity/item)
   (#:transaction #:kakeibo/entity/transaction)
   (#:valid #:kakeibo/global/valid)
   (#:iter #:coalton-library/iterator)
   (#:map #:coalton-library/ord-map)
   (#:result/trans #:kakeibo/global/result/trans)
   (#:monad/trans #:kakeibo/global/monad/trans))
  (:export #:init))
