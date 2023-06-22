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
   (#:exception #:kakeibo/global/exception))
  (:export
   #:Record
   #:Read
   #:ReadError #:NotFound

   #:Search
   #:SearchCondition

   #:amount))

(cl:in-package #:kakeibo/use-case/transaction-with-items)

(coalton-toplevel
  (define-type (Record :id :itemId)
    (Record (transaction:Transaction :id)
            (List (item:Item :itemId :id))))

  (define-type ReadError NotFound)
  (exception:define-exception-instance ReadError)

  (define-class (Monad :m => Read :m :id :itemId (:m -> :id :itemId))
    (read (:id -> result/t:ResultT ReadError :m (Record :id :itemId))))

  (define-type SearchCondition
    (SearchCondition
     (Optional date:date)               ; start date
     (Optional date:Date)               ; end date
     (Optional String)                  ; note
     ))

  (define-class (Monad :m => Search :m :id :itemId (:m -> :id :itemId))
    (search (SearchCondition
             -> :m (iter:Iterator (Record :id :itemId)))))

  (declare amount (Record :id :itemId -> Integer))
  (define (amount (Record _ items))
    (sum (map item:get-amount items))))
