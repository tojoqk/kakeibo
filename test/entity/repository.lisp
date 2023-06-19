(defpackage #:kakeibo/test/entity/repository
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:date #:kakeibo/entity/date)
   (#:transaction #:kakeibo/entity/transaction)
   (#:item #:kakeibo/entity/item)
   (#:valid #:kakeibo/global/valid)
   (#:result #:coalton-library/result)
   (#:result/t #:kakeibo/global/result/trans)
   (#:trans #:kakeibo/global/monad/trans)
   (#:exception #:kakeibo/global/exception))
  (:export
   #:test-transaction-create
   #:test-transaction-read
   #:test-transaction-update
   #:test-transaction-delete
   #:test-transaction-different-ids
   #:test-transaction-NotFoundOnRead-error
   #:test-transaction-NotFoundOnUpdate-error
   #:test-transaction-NotFoundOnDelete-error
   #:test-transaction-AssociatedItemsExist-error

   #:test-item-create
   #:test-item-read
   #:test-item-update
   #:test-item-delete
   #:test-item-different-ids
   #:test-item-TransactionNotFoundOnCreate-error
   #:test-item-NotFoundOnRead-error
   #:test-item-NotFoundOnUpdate-error
   #:test-item-TransactionNotFoundOnUpdate-error
   #:test-item-NotFoundOnDelete-error))

(in-package #:kakeibo/test/entity/repository)

(coalton-toplevel
  (define it/trx
    (transaction:transaction transaction:Income
                             (date:Date 2023 date:January 1)
                             (Some "Note")))

  (define (it/itm tid)
    (item:item tid
               "Cateogry"
               (Some "Subcategory")
               100
               (Some "Note")))

  (define (valid x)
    (nest result/t:T
          pure valid:valid x))

  (define (to-test m)
    (map (fn (res)
           (match res
             ((Ok b) b)
             ((Err _) False)))
         m))

  (define (test-transaction-create)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (trans:lift (transaction:create v))
       (pure True)))))

  (define (test-transaction-read)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (id <- (trans:lift (transaction:create v)))
        (trx <- (result/t:exception (transaction:read id)))
        (pure
         (and (== id (transaction:get-id trx))
              (== (transaction:get-type it/trx)
                  (transaction:get-type trx))
              (== (transaction:get-date it/trx)
                  (transaction:get-date trx))
              (== (transaction:get-note it/trx)
                  (transaction:get-note trx))))))))

  (define (test-transaction-update)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (id <- (trans:lift (transaction:create v)))
       (trx <- (result/t:exception (transaction:read id)))
       (v <- (result/t:exception
              (valid (transaction:update-note (Some "Note2")
                                              trx))))
       (result/t:exception (transaction:update v))
       (trx <- (result/t:exception (transaction:read id)))
       (pure
        (and (== id
                 (transaction:get-id trx))
             (== (transaction:get-type it/trx)
                 (transaction:get-type trx))
             (== (transaction:get-date it/trx)
                 (transaction:get-date trx))
             (/= (transaction:get-note it/trx)
                 (transaction:get-note trx))
             (== (Some "Note2")
                 (transaction:get-note trx))))))))

  (define (test-transaction-delete)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (id <- (trans:lift (transaction:create v)))
       (result/t:exception (transaction:delete id))
       (pure True)))))

  (define (test-transaction-different-ids)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (id1 <- (trans:lift (transaction:create v)))
       (id2 <- (trans:lift (transaction:create v)))
       (pure (/= id1 id2))))))

  (define (test-transaction-NotFoundOnRead-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (transaction:NotFoundOnRead))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (id <- (trans:lift (transaction:create v)))
           (result/t:exception (transaction:delete id))
           (result/t:exception (transaction:read id))))))

  (define (test-transaction-NotFoundOnUpdate-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (transaction:NotFoundOnUpdate))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (id <- (trans:lift (transaction:create v)))
           (trx <- (result/t:exception (transaction:read id)))
           (v <- (result/t:exception (valid trx)))
           (result/t:exception (transaction:delete id))
           (result/t:exception (transaction:update v))))))

  (define (test-transaction-NotFoundOnDelete-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (transaction:NotFoundOnDelete))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (id <- (trans:lift (transaction:create v)))
           (result/t:exception (transaction:delete id))
           (result/t:exception (transaction:delete id))))))

  (define (test-transaction-AssociatedItemsExist-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (transaction:AssociatedItemsExist))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (id <- (trans:lift (transaction:create v)))
           (v <- (result/t:exception (valid (it/itm id))))
           (result/t:exception (item:create v))
           (result/t:exception (transaction:delete id))))))

  (define (test-item-create)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
       (v <- (result/t:exception (valid (it/itm tid))))
       (result/t:exception (item:create v))
       (pure True)))))

  (define (test-item-read)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
       (v <- (result/t:exception (valid (it/itm tid))))
       (id <- (result/t:exception (item:create v)))
       (itm <- (result/t:exception (item:read id)))
       (pure
        (and (== id (item:get-id itm))
             (== tid
                 (item:get-transaction-id itm))
             (== (item:get-category (it/itm tid))
                 (item:get-category itm))
             (== (item:get-subcategory (it/itm tid))
                 (item:get-subcategory itm))
             (== (item:get-amount (it/itm tid))
                 (item:get-amount itm))
             (== (item:get-note (it/itm tid))
                 (item:get-note itm))))))))

  (define (test-item-update)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
       (v <- (result/t:exception (valid (it/itm tid))))
       (id <- (result/t:exception (item:create v)))
       (itm <- (result/t:exception (item:read id)))
       (v <- (result/t:exception
              (valid
               (item:update-amount 9999 itm))))
       (result/t:exception (item:update v))
       (itm <- (result/t:exception (item:read id)))
       (pure
        (and (== id (item:get-id itm))
             (== tid
                 (item:get-transaction-id itm))
             (== (item:get-category (it/itm tid))
                 (item:get-category itm))
             (== (item:get-subcategory (it/itm tid))
                 (item:get-subcategory itm))
             (/= (item:get-amount (it/itm tid))
                 (item:get-amount itm))
             (== 9999
                 (item:get-amount itm))
             (== (item:get-note (it/itm tid))
                 (item:get-note itm))))))))

  (define (test-item-delete)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
       (v <- (result/t:exception (valid (it/itm tid))))
       (id <- (result/t:exception (item:create v)))
       (result/t:exception (item:delete id))
       (pure True)))))

  (define (test-item-different-ids)
    (to-test
     (result/t:run
      (do
       (v <- (result/t:exception (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
       (v <- (result/t:exception (valid (it/itm tid))))
       (id1 <- (result/t:exception (item:create v)))
       (id2 <- (result/t:exception (item:create v)))
       (pure (/= id1 id2))))))

  (define (test-item-TransactionNotFoundOnCreate-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (item:TransactionNotFoundOnCreate))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (tid <- (trans:lift (transaction:create v)))
           (result/t:exception (transaction:delete tid))
           (v <- (result/t:exception (valid (it/itm tid))))
           (result/t:exception (item:create v))))))

  (define (test-item-NotFoundOnRead-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (item:NotFoundOnRead))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (tid <- (trans:lift (transaction:create v)))
           (v <- (result/t:exception (valid (it/itm tid))))
           (id <- (result/t:exception (item:create v)))
           (result/t:exception (item:delete id))
           (result/t:exception (item:read id))))))

  (define (test-item-NotFoundOnUpdate-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (item:NotFoundOnUpdate))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (tid <- (trans:lift (transaction:create v)))
           (v <- (result/t:exception (valid (it/itm tid))))
           (id <- (result/t:exception (item:create v)))
           (itm <- (result/t:exception (item:read id)))
           (result/t:exception (item:delete id))
           (v <- (result/t:exception (valid itm)))
           (result/t:exception (item:update v))))))

  (define (test-item-TransactionNotFoundOnUpdate-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (item:NotFoundOnUpdate))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (tid <- (trans:lift (transaction:create v)))
           (v <- (result/t:exception (valid (it/itm tid))))
           (id <- (result/t:exception (item:create v)))
           (itm <- (result/t:exception (item:read id)))
           (result/t:exception (item:delete id))
           (v <- (result/t:exception (valid itm)))
           (result/t:exception (item:update v))))))

  (define (test-item-NotFoundOnDelete-error)
    (map (fn (res)
           (match (result:map-err exception:from res)
             ((Err (Some (item:NotFoundOnDelete))) True)
             (_ False)))
         (result/t:run
          (do
           (v <- (result/t:exception (valid it/trx)))
           (tid <- (trans:lift (transaction:create v)))
           (v <- (result/t:exception (valid (it/itm tid))))
           (id <- (result/t:exception (item:create v)))
           (result/t:exception (item:delete id))
           (result/t:exception (item:delete id)))))))
