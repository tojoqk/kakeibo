(in-package #:kakeibo/test/use-case/repository)

(coalton-toplevel
  (define (test-item-with-transaction-read)
    (to-test
     (result/t:run
      (do
       (trx <- (it/trx))
       (tid <- (>>= (valid trx) (.< trans:lift trx:create)))
       (let itm1 = (itm:update-category "Item1" (it/itm tid)))
       (let itm2 = (itm:update-category "Item2" (it/itm tid)))
       (let itm3 = (itm:update-category "Item3" (it/itm tid)))
       (id <- (>>= (valid itm1) (.< result/t:some itm:create)))
       (_ <- (>>= (valid itm2) (.< result/t:some itm:create)))
       (_ <- (>>= (valid itm3) (.< result/t:some itm:create)))
       (rec <- (result/t:some (itm/trx:read id)))
       (pure (and (== id (itm:get-id (itm/trx:item rec)))
                  (== tid (trx:get-id (itm/trx:transaction rec)))))))))

  (define (test-item-with-transaction-amount/income)
    (to-test
     (result/t:run
      (do
       (trx <- (map (trx:update-type type:Income) (it/trx)))
       (tid <- (>>= (valid trx) (.< trans:lift trx:create)))
       (id <- (>>= (valid (itm:update-amount 1234 (it/itm tid)))
                   (.< result/t:some itm:create)))
       (rec <- (result/t:some (itm/trx:read id)))
       (pure (== (yen:Yen 1234) (itm/trx:amount rec)))))))

  (define (test-item-with-transaction-amount/outgo)
    (to-test
     (result/t:run
      (do
       (trx <- (map (trx:update-type type:Outgo) (it/trx)))
       (tid <- (>>= (valid trx) (.< trans:lift trx:create)))
       (id <- (>>= (valid (itm:update-amount 1234 (it/itm tid)))
                   (.< result/t:some itm:create)))
       (rec <- (result/t:some (itm/trx:read id)))
       (pure (== (yen:Yen -1234) (itm/trx:amount rec))))))))
