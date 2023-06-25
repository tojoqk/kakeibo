(in-package #:kakeibo/repository/tree)

(coalton-toplevel
  (define-type Tree
    (%Tree Integer                      ; latest transaction id
           Integer                      ; latest item id
           (map:Map TransactionId
                    (Tuple (transaction:Transaction TransactionId)
                           (map:Map ItemId (item:Item ItemId TransactionId))))))

  (define init
    (%Tree 1
           1
           map:empty))

  (repr :transparent)
  (define-type TransactionId (TransactionId Integer))

  (define-instance (Eq TransactionId)
    (define (== (TransactionId x) (TransactionId y))
      (== x y)))

  (define-instance (Ord TransactionId)
    (define (<=> (TransactionId x) (TransactionId y))
      (<=> y x)))

  (define (generate-transaction-id)
    (do
     ((%Tree current-trx-id next-itm-id mp) <- st:get)
     (st:put (%Tree (1+ current-trx-id) next-itm-id mp))
      (pure (TransactionId current-trx-id))))

  (define-type ItemId (ItemId TransactionId Integer))

  (define-instance (Eq ItemId)
    (define (== (ItemId trx-id1 id1) (ItemId trx-id2 id2))
      (and (== trx-id1 trx-id2)
           (== id1 id2))))

  (define-instance (Ord ItemId)
    (define (<=> (ItemId trx-id1 id1) (ItemId trx-id2 id2))
      (match (<=> trx-id1 trx-id2)
        ((EQ) (<=> id1 id2))
        (x x))))

  (define (generate-item-id trx-id)
    (do
     ((%Tree next-trx-id current-itm-id  mp) <- st:get)
     (st:put (%Tree next-trx-id (1+ current-itm-id) mp))
      (pure (ItemId trx-id current-itm-id))))

  (define (insert-transaction trx)
    (do
     ((%Tree next-trx-id next-itm-id trx-mp) <- st:get)
     (st:put
      (%Tree next-trx-id next-itm-id
             (map:insert-or-replace trx-mp (transaction:get-id trx) (Tuple trx map:empty))))))

  (define (find-transaction trx-id)
    (do
     ((%Tree _next-trx-id _next-itm-id trx-mp) <- st:get)
     (pure (map:lookup trx-mp trx-id))))

  (define (update-transaction trx)
    (do
     ((%Tree next-trx-id next-itm-id trx-mp) <- (trans:lift st:get))
     (match (map:lookup trx-mp (transaction:get-id trx))
       ((Some (Tuple _trx itm-mp))
        (do
         (trans:lift
          (st:put (%Tree next-trx-id next-itm-id
                         (map:insert-or-replace trx-mp
                                                (transaction:get-id trx)
                                                (Tuple trx itm-mp)))))
         (pure Unit)))
       ((None)
        (result/t:hoist (Err transaction:NotFoundOnUpdate))))))

  (define (delete-transaction trx-id)
    (do
     ((%Tree next-trx-id next-itm-id trx-mp) <- (trans:lift st:get))
     (match (map:lookup trx-mp trx-id)
       ((Some (Tuple _trx itm-mp))
        (if (< 0 (iter:count! (map:keys itm-mp)))
            (result/t:hoist
             (Err transaction:AssociateditemsExist))
            (match (map:remove trx-mp trx-id)
              ((Some trx-mp)
               (trans:lift (st:put
                            (%Tree next-trx-id next-itm-id trx-mp))))
              ((None)
               (result/t:hoist
                (Err transaction:NotFoundOnDelete))))))
       ((None)
        (result/t:hoist (Err transaction:NotFoundOnDelete))))))

  (define (get-trx-id (ItemId trx-id _)) trx-id)

  (define (insert-item itm)
    (let itm-id = (item:get-id itm))
    (let trx-id = (get-trx-id itm-id))
    (do
     ((%Tree next-trx-id next-itm-id trx-mp) <- (trans:lift st:get))
     (match (map:lookup trx-mp trx-id)
       ((Some (Tuple trx itm-mp))
        (trans:lift
         (st:put
          (%Tree next-trx-id
                 next-itm-id
                 (map:insert-or-replace
                  trx-mp
                  trx-id
                  (Tuple trx
                         (map:insert-or-replace itm-mp
                                                itm-id
                                                itm)))))))
       ((None)
        (result/t:hoist
         (Err item:TransactionNotFoundOnCreate))))))

  (define (find-item itm-id)
    (let trx-id = (get-trx-id itm-id))
    (do
     ((%Tree _next-trx-id _next-itm-id trx-mp) <- st:get)
     (pure
      (match (map:lookup trx-mp trx-id)
        ((Some (Tuple trx itm-mp))
         (match (map:lookup itm-mp itm-id)
           ((Some itm)
            (Some (Tuple trx itm)))
           ((None) None)))
        ((None) None)))))

  (define (update-item itm)
    (let itm-id = (item:get-id itm))
    (let trx-id = (get-trx-id itm-id))
    (do
     ((%Tree next-trx-id next-itm-id trx-mp) <- (trans:lift st:get))
     (match (map:lookup trx-mp trx-id)
       ((Some (Tuple trx itm-mp))
        (match (map:lookup itm-mp itm-id)
          ((Some _)
           (trans:lift
            (st:put
             (%Tree next-trx-id
                    next-itm-id
                    (map:insert-or-replace
                     trx-mp
                     trx-id
                     (Tuple trx
                            (map:insert-or-replace itm-mp
                                                   itm-id
                                                   itm)))))))
          ((None)
           (result/t:hoist
            (Err item:NotFoundOnUpdate)))))
       ((None)
        (result/t:hoist
         (Err item:TransactionNotFoundOnUpdate))))))

  (define (delete-item itm-id)
    (let trx-id = (get-trx-id itm-id))
    (do
     ((%Tree next-trx-id next-itm-id trx-mp) <- (trans:lift st:get))
     (match (map:lookup trx-mp trx-id)
       ((Some (Tuple trx itm-mp))
        (match (map:remove itm-mp itm-id)
          ((Some itm-mp)
           (trans:lift
            (st:put
             (%Tree next-trx-id
                    next-itm-id
                    (map:insert-or-replace
                     trx-mp
                     trx-id
                     (Tuple trx itm-mp))))))
          ((None)
           (result/t:hoist
            (Err item:NotFoundOnDelete)))))
       ((None)
        (result/t:hoist
         (Err item:NotFoundOnDelete)))))))
