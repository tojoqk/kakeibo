(cl:defpackage #:kakeibo/use-case/item-with-transaction
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:trx #:kakeibo/entity/transaction)
   (#:itm #:kakeibo/entity/item)
   (#:amount #:kakeibo/use-case/amount)
   (#:result/t #:kakeibo/global/result/trans)
   (#:date #:kakeibo/global/date)
   (#:exception #:kakeibo/global/exception))
  (:export
   #:Record
   #:Read %read
   #:ReadError #:NotFound

   #:transaction #:item

   #:amount))

(cl:in-package #:kakeibo/use-case/item-with-transaction)

(coalton-toplevel
  (define-type (Record :id :tid)
    (%Record (itm:Item :id :tid)
             (trx:Transaction :tid)))

  (define (item (%Record itm _)) itm)
  (define (transaction (%Record _ trx)) trx)

  (define-type ReadError NotFound)
  (exception:define-exception-instance ReadError)

  (define-class (Monad :m => Read :m :id :tid (:m -> :id :tid))
    (%read (:id
            -> (result/t:ResultT ReadError :m
                                 (Tuple (itm:Item :id :tid)
                                        (trx:Transaction :tid))))))
    
  (declare read (Read :m :id :tid => :id -> result/t:ResultT ReadError :m (Record :id :tid)) )
  (define (read id)
    (do ((Tuple itm trx) <- (%read id))
        (pure (%Record itm trx))))

  (declare amount (amount:Amount :a => Record :id :tid -> :a))
  (define (amount (%Record itm trx))
    (amount:amount (trx:get-type trx)
                   (itm:get-amount itm))))
