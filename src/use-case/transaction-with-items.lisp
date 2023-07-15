(cl:defpackage #:kakeibo/use-case/transaction-with-items
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:transaction #:kakeibo/entity/transaction)
   (#:item #:kakeibo/entity/item)
   (#:amount #:kakeibo/use-case/amount)
   (#:result/t #:kakeibo/global/result/trans)
   (#:date #:kakeibo/global/date)
   (#:exception #:kakeibo/global/exception))
  (:export
   #:Record
   #:Read %read
   #:ReadError #:NotFound
   #:transaction #:items

   #:Search %search
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
     (Optional date:Date)               ; start date
     (Optional date:Date)               ; end date
     (Optional String)                  ; note
     ))

  (define-class (Monad :m => Search :m :id :itemId (:m -> :id :itemId))
    (%search (SearchCondition
              -> UFix ; offset
              -> UFix ; limit
              -> :m (iter:Iterator
                     (Tuple (transaction:Transaction :id)
                            (List (item:Item :itemId :id)))))))

  (declare search (Search :m :id :itemId
                          => SearchCondition
                          -> UFix
                          -> UFix
                          -> :m (iter:Iterator (Record :id :itemId))))
  (define (search sc offset limit)
    (do (iter <- (%search sc offset limit))
        (pure (map (fn ((Tuple trx itms))
                     (%Record trx itms))
                   iter))))

  (declare amount (amount:Amount :a => Record :id :itemId -> :a))
  (define (amount (%Record trx itms))
    (mconcat (map (.< (amount:amount (transaction:get-type trx))
                      item:get-amount)
                  itms))))
