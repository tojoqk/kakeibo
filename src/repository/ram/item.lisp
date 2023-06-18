(in-package #:kakeibo/repository/ram)

(coalton-toplevel
  (define (check-item-exists id e)
    (do
     ((%RAM _ (Item _ itm-mp)) <- (monad/trans:lift st:get))
     (match (map:lookup itm-mp id)
       ((Some _) (pure Unit))
       ((None)
        (result/trans:T (pure (Err e)))))))

  (define-instance (item:Creatable (st:ST RAM) Integer Integer)
    (define (item:create itm)
      (let itm = (valid:get itm))
      (do
       ((%RAM (Transaction trx-id trx-mp) (Item current-id mp)) <- (monad/trans:lift st:get))
       (itm <-  (item:%set-id current-id itm))
       (check-transaction-exists (item:get-transaction-id itm)
                                 item:TransactionNotFoundOnCreate)
       (monad/trans:lift
        (st:put (%RAM (Transaction trx-id trx-mp)
                      (Item (1+ current-id)
                            (map:insert-or-replace mp current-id itm)))))
       (pure current-id))))

  (define-instance (item:Readable (st:ST RAM) Integer Integer)
    (define (item:read id)
      (do ((%RAM trx (Item next-id mp)) <- (monad/trans:lift st:get))
          (match (map:lookup mp id)
            ((Some itm) (pure itm))
            ((None)
             (result/trans:T (pure (Err item:NotFoundOnRead))))))))

  (define-instance (item:Updatable (st:ST RAM) Integer Integer)
    (define (item:update itm)
      (let itm = (valid:get itm))
      (let id = (item:get-id itm))
      (do
       ((%RAM trx (Item next-id mp)) <- (monad/trans:lift st:get))
       (check-item-exists id item:NotFoundOnUpdate)
       (check-transaction-exists (item:get-transaction-id itm)
                                 item:TransactionNotFoundOnUpdate)
       (monad/trans:lift
        (st:put (%RAM trx
                      (Item next-id
                            (map:insert-or-replace mp
                                                   id
                                                   itm))))))))

  (define-instance (item:Deletable (st:ST RAM) Integer)
    (define (item:delete id)
      (do
       ((%RAM trx (Item itm-id itm-mp)) <- (monad/trans:lift st:get))
       (match (map:remove itm-mp id)
         ((Some new-mp)
          (monad/trans:lift (st:put (%RAM trx (Item itm-id itm-mp)))))
         ((None)
          (result/trans:T
           (pure (Err item:NotFoundOnDelete)))))))))
