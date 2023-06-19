(in-package #:kakeibo/repository/ram)

(coalton-toplevel
  (define (check-transaction-exists id e)
    (do ((%RAM (Transaction _ trx-mp) _) <- (monad/trans:lift st:get))
        (match (map:lookup trx-mp id)
          ((Some _) (pure Unit))
          ((None)
           (result/trans:ResultT (pure (Err e)))))))

  (define (check-no-associated-items-in-transaction trx-id e)
    (do ((%RAM _ (Item _ itm-mp)) <- (monad/trans:lift st:get))
        (if (iter:and! (map (.< (/= trx-id) item:get-transaction-id)
                            (map:values itm-mp)))
            (pure Unit)
            (result/trans:ResultT (pure (Err e))))))

  (define-instance (transaction:Creatable (st:ST RAM) Integer)
    (define (transaction:create trx)
      (let trx = (valid:get trx))
      (do
       ((%RAM (Transaction current-id mp) itm) <- st:get)
       (trx <- (transaction:%set-id current-id trx))
        (st:put (%RAM (Transaction (1+ current-id)
                                   (map:insert-or-replace mp current-id trx))
                      itm))
       (pure current-id))))

  (define-instance (transaction:Readable (st:ST RAM) Integer)
    (define (transaction:read id)
      (do
       ((%RAM (Transaction next-id mp) itm) <- (monad/trans:lift st:get))
       (match (map:lookup mp id)
         ((Some trx) (pure trx))
         ((None)
          (result/trans:ResultT (pure (Err transaction:NotFoundOnRead))))))))

  (define-instance (transaction:Updatable (st:ST RAM) Integer)
    (define (transaction:update trx)
      (let trx = (valid:get trx))
      (let id = (transaction:get-id trx))
      (do
       ((%RAM (Transaction next-id mp) itm) <- (monad/trans:lift st:get))
       (check-transaction-exists id transaction:NotFoundOnUpdate)
       (monad/trans:lift
        (st:put (%RAM (Transaction next-id
                                   (map:insert-or-replace mp id trx))
                      itm))))))

  (define-instance (transaction:Deletable (st:ST RAM) Integer)
    (define (transaction:delete id)
      (do
       ((%RAM (Transaction next-id trx-mp) itm) <- (monad/trans:lift st:get))
       (match (map:remove trx-mp id)
         ((Some new-mp)
          (do (check-no-associated-items-in-transaction
               id
               transaction:AssociatedItemsExist)
              (monad/trans:lift (st:put (%RAM (Transaction next-id new-mp)
                                              itm)))))
         ((None)
          (result/trans:ResultT
           (pure (Err transaction:NotFoundOnDelete)))))))))
