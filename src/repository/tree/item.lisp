(in-package #:kakeibo/repository/tree)

(coalton-toplevel
  (define-instance (item:Creatable (st:ST Tree) ItemId TransactionId)
    (define (item:create itm)
      (let itm = (valid:get itm))
      (let trx-id = (item:get-transaction-id itm))
      (do
       (itm-id <- (trans:lift (generate-item-id trx-id)))
       (itm <- (trans:lift (item:%set-id itm-id itm)))
       (insert-item itm)
       (pure itm-id))))

  (define-instance (item:Readable (st:ST Tree) ItemId TransactionId)
    (define (item:read itm-id)
      (do
       (opt <- (trans:lift (find-item itm-id)))
       (match opt
         ((Some (Tuple _ itm))
          (pure itm))
         ((None)
          (result/t:hoist (Err item:NotFoundOnRead)))))))

  (define-instance (item:Updatable (st:ST Tree) ItemId TransactionId)
    (define (item:update itm)
      (let itm = (valid:get itm))
      (update-item itm)))

  (define-instance (item:Deletable (st:ST Tree) ItemId)
    (define (item:delete itm-id)
      (delete-item itm-id))))
