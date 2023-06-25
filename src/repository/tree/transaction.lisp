(in-package #:kakeibo/repository/tree)

(coalton-toplevel
  (define-instance (transaction:Creatable (st:ST Tree) TransactionId)
    (define (transaction:create trx)
      (let trx = (valid:get trx))
      (do
       (trx-id <- (generate-transaction-id))
       (trx <- (transaction:%set-id trx-id trx))
       (insert-transaction trx)
       (pure trx-id))))

  (define-instance (transaction:Readable (st:ST Tree) TransactionId)
    (define (transaction:read trx-id)
      (do
       (opt <- (trans:lift (find-transaction trx-id)))
       (match opt
         ((Some (Tuple trx _))
          (pure trx))
         ((None)
          (result/t:hoist (Err transaction:NotFoundOnRead)))))))

  (define-instance (transaction:Updatable (st:ST Tree) TransactionId)
    (define (transaction:update trx)
      (let trx = (valid:get trx))
      (update-transaction trx)))

  (define-instance (transaction:Deletable (st:ST Tree) TransactionId)
    (define (transaction:delete trx-id)
      (delete-transaction trx-id))))
