(in-package #:kakeibo/repository/tree)

(coalton-toplevel
  (define-instance (itm/trx:Read (st:ST Tree) ItemId TransactionId)
    (define (itm/trx:%read id)
      (do
       (let trx-id = (get-trx-id id))
       (opt <- (trans:lift (find-transaction trx-id)))
       (match opt
         ((Some (Tuple trx itm-mp))
          (match (map:lookup itm-mp id)
            ((Some itm)
             (pure (Tuple itm trx)))
            ((None)
             (result/t:hoist (Err itm/trx:NotFound)))))
         ((None)
          (result/t:hoist (Err itm/trx:NotFound))))))))
