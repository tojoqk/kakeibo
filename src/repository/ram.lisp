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

(in-package #:kakeibo/repository/ram)

(coalton-toplevel
  (define-type Transaction
    (Transaction Integer (map:Map Integer (transaction:Transaction Integer))))

  (define-type Item
    (Item Integer (map:Map Integer (item:Item Integer Integer))))

  (define-type RAM
    (%RAM Transaction Item))

  (define (transaction-not-found-error id e)
    (do ((%RAM (Transaction _ trx-mp) _) <- (lift st:get))
        (match (map:lookup trx-mp id)
          ((Some _) (pure Unit))
          ((None)
           (ResultT (pure (Err e)))))))

  (define (item-not-found-error id e)
    (do ((%RAM _ (Item _ itm-mp)) <- (lift st:get))
        (match (map:lookup itm-mp id)
          ((Some _) (pure Unit))
          ((None)
           (ResultT (pure (Err e)))))))

  (define (associated-items-exist-error trx-id e)
    (do ((%RAM _ (Item _ itm-mp)) <- (lift st:get))
        (if (iter:and! (map (.< (/= trx-id) item:get-transaction-id)
                            (map:values itm-mp)))
            (pure Unit)
            (ResultT (pure (Err e))))))

  (define-instance (transaction:Creatable (st:ST RAM) Integer)
    (define (transaction:create trx)
      (let trx = (valid:get trx))
      (do ((%RAM (Transaction current-id mp) itm) <- st:get)
          (trx <- (transaction:%set-id current-id trx))
        (st:put (%RAM (Transaction (1+ current-id)
                                   (map:insert-or-replace mp current-id trx))
                      itm))
        (pure current-id))))

  (define-instance (transaction:Readable (st:ST RAM) Integer)
    (define (transaction:read id)
      (do ((%RAM (Transaction next-id mp) itm) <- (lift st:get))
          (match (map:lookup mp id)
            ((Some trx) (pure trx))
            ((None)
             (ResultT (pure (Err transaction:NotFoundOnRead))))))))

  (define-instance (transaction:Updatable (st:ST RAM) Integer)
    (define (transaction:update trx)
      (let trx = (valid:get trx))
      (let id = (transaction:get-id trx))
      (do ((%RAM (Transaction next-id mp) itm) <- (lift st:get))
          (transaction-not-found-error id transaction:NotFoundOnUpdate)
        (lift (st:put (%RAM (Transaction next-id
                                         (map:insert-or-replace mp id trx))
                            itm))))))

  (define-instance (transaction:Deletable (st:ST RAM) Integer)
    (define (transaction:delete id)
      (do ((%RAM (Transaction next-id trx-mp) itm) <- (lift st:get))
          (match (map:remove trx-mp id)
            ((Some new-mp)
             (do (associated-items-exist-error id
                                               transaction:AssociatedItemsExist)
                 (lift (st:put (%RAM (Transaction next-id new-mp)
                                     itm)))))
            ((None)
             (ResultT (pure (Err transaction:NotFoundOnDelete))))))))

  (define-instance (item:Creatable (st:ST RAM) Integer Integer)
    (define (item:create itm)
      (let itm = (valid:get itm))
      (do ((%RAM (Transaction trx-id trx-mp) (Item current-id mp)) <- (lift st:get))
          (itm <-  (item:%set-id current-id itm))
        (transaction-not-found-error (item:get-transaction-id itm)
                                     item:TransactionNotFoundOnCreate)
        (lift (st:put (%RAM (Transaction trx-id trx-mp)
                            (Item (1+ current-id)
                                  (map:insert-or-replace mp current-id itm)))))
        (pure current-id))))

  (define-instance (item:Readable (st:ST RAM) Integer Integer)
    (define (item:read id)
      (do ((%RAM trx (Item next-id mp)) <- (lift st:get))
          (match (map:lookup mp id)
            ((Some itm) (pure itm))
            ((None)
             (ResultT (pure (Err item:NotFoundOnRead))))))))

  (define-instance (item:Updatable (st:ST RAM) Integer Integer)
    (define (item:update itm)
      (let itm = (valid:get itm))
      (let id = (item:get-id itm))
      (do ((%RAM trx (Item next-id mp)) <- (lift st:get))
          (item-not-found-error id item:NotFoundOnUpdate)
          (transaction-not-found-error (item:get-transaction-id itm)
                                       item:TransactionNotFoundOnUpdate)
          (lift (st:put (%RAM trx
                              (Item next-id
                                    (map:insert-or-replace mp
                                                           id
                                                           itm))))))))

  (define-instance (item:Deletable (st:ST RAM) Integer)
    (define (item:delete id)
      (do ((%RAM trx (Item itm-id itm-mp)) <- (lift st:get))
          (match (map:remove itm-mp id)
            ((Some new-mp)
             (lift (st:put (%RAM trx (Item itm-id itm-mp)))))
            ((None)
             (ResultT (pure (Err item:NotFoundOnDelete)))))))))
