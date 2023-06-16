(in-package #:kakeibo/repository/ram)

(coalton-toplevel
  (define (item-not-found-error id e)
    (do ((%RAM _ (Item _ itm-mp)) <- (lift st:get))
        (match (map:lookup itm-mp id)
          ((Some _) (pure Unit))
          ((None)
           (ResultT (pure (Err e)))))))

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
