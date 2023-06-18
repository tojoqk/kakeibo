(defpackage #:kakeibo/test/entity/repository
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:date #:kakeibo/entity/date)
   (#:transaction #:kakeibo/entity/transaction)
   (#:item #:kakeibo/entity/item)
   (#:valid #:kakeibo/global/valid)
   (#:result #:coalton-library/result)
   (#:result/trans #:kakeibo/global/result/trans)
   (#:trans #:kakeibo/global/monad/trans))
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
   #:test-item-delete
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

  (define (ignore-err m)
    (result/trans:T
     (map (result:map-err (const Unit))
          (result/trans:run m))))

  (define (valid x)
    (nest result/trans:T
          pure valid:valid x))

  (define (to-test m)
    (map (fn (res)
           (match res
             ((Ok b) b)
             ((Err _) False)))
         m))

  (define (test-transaction-create)
    (to-test
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (trans:lift (transaction:create v))
       (pure True)))))

  (define (test-transaction-read)
    (to-test
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (id <- (trans:lift (transaction:create v)))
       (trx <- (ignore-err (transaction:read id)))
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
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (id <- (trans:lift (transaction:create v)))
       (trx <- (ignore-err (transaction:read it/trx)))
       (v <- (ignore-err
              (valid (transaction:update-note (Some "Note2")
                                              trx))))
       (ignore-err (transaction:update v))
       (trx <- (ignore-err (transaction:read it/trx)))
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
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (id <- (trans:lift (transaction:create v)))
       (ignore-err (transaction:delete id))
       (pure True)))))

  (define (test-transaction-different-ids)
    (to-test
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (id1 <- (trans:lift (transaction:create v)))
       (id2 <- (trans:lift (transaction:create v)))
       (pure (/= id1 id2))))))

  (define (test-transaction-NotFoundOnRead-error)
    (map (fn (res)
           (match res
             ((Err (transaction:NotFoundOnRead)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (id <- (trans:lift (transaction:create v)))
                    (ignore-err (transaction:delete id))
                    (pure id)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok id)
              (do
               (transaction:read id)
               (pure Unit))))))))

  (define (test-transaction-NotFoundOnUpdate-error)
    (map (fn (res)
           (match res
             ((Err (transaction:NotFoundOnUpdate)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (id <- (trans:lift (transaction:create v)))
                   (ignore-err (transaction:delete id))
                   (pure v)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok v)
              (do
               (transaction:update v)
               (pure Unit))))))))

  (define (test-transaction-NotFoundOnDelete-error)
    (map (fn (res)
           (match res
             ((Err (transaction:NotFoundOnDelete)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (id <- (trans:lift (transaction:create v)))
                   (ignore-err (transaction:delete id))
                   (pure id)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok v)
              (do
               (transaction:delete v)
               (pure Unit))))))))

  (define (test-transaction-AssociatedItemsExist-error)
    (map (fn (res)
           (match res
             ((Err (transaction:AssociatedItemsExist)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (id <- (trans:lift (transaction:create v)))
                   (v <- (ignore-err (valid (it/itm id))))
                   (ignore-err (item:create v))
                   (pure id)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok id)
              (do
               (transaction:delete id)
               (pure Unit))))))))

  (define (test-item-create)
    (to-test
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
       (v <- (ignore-err (valid (it/itm tid))))
       (ignore-err (item:create v))
       (pure True)))))

  (define (test-item-read)
    (to-test
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
        (v <- (ignore-err (valid (it/itm tid))))
        (id <- (ignore-err (item:create v)))
        (itm <- (ignore-err (item:read v)))
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
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
        (v <- (ignore-err (valid (it/itm tid))))
        (id <- (ignore-err (item:create v)))
        (itm <- (ignore-err (item:read v)))
        (v <- (ignore-err
               (valid
                (item:update-amount 9999 itm))))
        (ignore-err (item:update v))
        (itm <- (ignore-err (item:read v)))
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
     (result/trans:run
      (do
       (v <- (ignore-err (valid it/trx)))
       (tid <- (trans:lift (transaction:create v)))
        (v <- (ignore-err (valid (it/itm tid))))
        (id <- (ignore-err (item:create v)))
        (ignore-err (item:delete id))
        (pure True)))))

  (define (test-item-TransactionNotFoundOnCreate-error)
    (map (fn (res)
           (match res
             ((Err (item:TransactionNotFoundOnCreate)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (tid <- (trans:lift (transaction:create v)))
                   (ignore-err (transaction:delete tid))
                   (v <- (ignore-err (valid (it/itm tid))))
                   (pure v)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok v)
              (do
               (item:create v)
               (pure Unit))))))))

  (define (test-item-NotFoundOnRead-error)
    (map (fn (res)
           (match res
             ((Err (item:NotFoundOnRead)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (tid <- (trans:lift (transaction:create v)))
                   (v <- (ignore-err (valid (it/itm tid))))
                   (id <- (ignore-err (item:create v)))
                   (ignore-err (item:delete id))
                   (pure id)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok id)
              (do
               (item:read id)
               (pure Unit))))))))

  (define (test-item-NotFoundOnUpdate-error)
    (map (fn (res)
           (match res
             ((Err (item:NotFoundOnUpdate)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (tid <- (trans:lift (transaction:create v)))
                    (v <- (ignore-err (valid (it/itm tid))))
                    (id <- (ignore-err (item:create v)))
                    (itm <- (ignore-err (item:read id)))
                    (ignore-err (item:delete id))
                    (v <- (ignore-err (valid itm)))
                    (pure v)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok v)
              (do
               (item:update v)
               (pure Unit))))))))

  (define (test-item-TransactionNotFoundOnUpdate-error)
    (map (fn (res)
           (match res
             ((Err (item:NotFoundOnUpdate)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (tid <- (trans:lift (transaction:create v)))
                   (v <- (ignore-err (valid (it/itm tid))))
                   (id <- (ignore-err (item:create v)))
                   (itm <- (ignore-err (item:read id)))
                   (ignore-err (transaction:delete tid))
                   (v <- (ignore-err (valid itm)))
                   (pure v)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok v)
              (do
               (item:update v)
               (pure Unit))))))))

  (define (test-item-NotFoundOnDelete-error)
    (map (fn (res)
           (match res
             ((Err (item:NotFoundOnDelete)) True)
             (_ False)))
         (result/trans:run
          (do
           (res <-
                (trans:lift
                 (result/trans:run
                  (do
                   (v <- (ignore-err (valid it/trx)))
                   (tid <- (trans:lift (transaction:create v)))
                   (v <- (ignore-err (valid (it/itm tid))))
                   (id <- (ignore-err (item:create v)))
                   (ignore-err (item:delete id))
                   (pure id)))))
           (match res
             ((Err _) (pure Unit))
             ((Ok id)
              (do
               (item:delete id)
               (pure Unit)))))))))
