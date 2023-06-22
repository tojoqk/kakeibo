(cl:defpackage #:kakeibo/use-case/transaction-with-items
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:transaction #:kakeibo/entity/transaction)
   (#:item #:kakeibo/entity/item)
   (#:result/t #:kakeibo/global/result/trans)
   (#:date #:kakeibo/global/date)
   (#:exception #:kakeibo/global/exception)))

(cl:in-package #:kakeibo/use-case/transaction-with-items)

(coalton-toplevel
  (define-type (Record :TransactionId
                       :ItemId)
    (Record (transaction:Transaction :TransactionId)
            (List (item:Item :ItemId :TransactionId))))

  (define-type ReadError NotFound)
  (exception:define-exception-instance ReadError)

  (define-class (Monad :m => Read :m :TransactionId :ItemId (:m -> :TransactionId :ItemId))
    (read (:TransactionId
           -> result/t:ResultT ReadError :m (Record :TransactionId :ItemId))))

  (define-type SearchCondition
    (SearchCondition
     (Optional date:date)               ; start date
     (Optional date:Date)               ; end date
     (Optional String)                  ; note
     ))

  (define-class (Monad :m => Search :m :TransactionId :ItemId (:m -> :TransactionId :ItemId))
    (search (SearchCondition
             -> :m (iter:Iterator (Record :TransactionId :ItemId))))))

