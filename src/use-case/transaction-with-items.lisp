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
   #:Read %read
   #:ReadError #:NotFound

   #:Search
   #:SearchCondition

   #:amount))

(cl:in-package #:kakeibo/use-case/transaction-with-items)

(coalton-toplevel
  (define-type (Record :id :itemId)
    (%Record (transaction:Transaction :id)
             (List (item:Item :itemId :id))))

  (define (transaction (%Record trx _)) trx)
  (define (items (%Record _ itms)) itms)

  (define-type ReadError NotFound)
  (exception:define-exception-instance ReadError)

  (define-class (Monad :m => Read :m :id :itemId (:m -> :id :itemId))
    (%read (:id
            -> (result/t:ResultT ReadError :m
                                 (Tuple (transaction:Transaction :id)
                                        (List (item:Item :itemId :id)))))))

  (declare read (Read :m :id :itemId => :id -> result/t:ResultT ReadError :m (Record :id :itemId)) )
  (define (read id)
    (do ((Tuple trx itms) <- (%read id))
        (pure (%Record trx itms))))

  (define-type SearchCondition
    (SearchCondition
     (Optional date:date)               ; start date
     (Optional date:Date)               ; end date
     (Optional String)                  ; note
     ))

  (define-class (Monad :m => Search :m :id :itemId (:m -> :id :itemId))
    (%search (SearchCondition
              -> :m (iter:Iterator
                     (Tuple (transaction:Transaction :id)
                            (List (item:Item :itemId :id)))))))

  (declare search (Search :m :id :itemId =>
                          SearchCondition
                          -> :m (iter:Iterator (Record :id :itemId))))
  (define (search sc)
    (do (iter <- (%search sc))
        (pure (map (fn ((Tuple trx itms))
                     (%Record trx itms))
                   iter))))

  (declare amount (Record :id :itemId -> Integer))
  (define (amount (%Record _ itms))
    (sum (map item:get-amount itms))))