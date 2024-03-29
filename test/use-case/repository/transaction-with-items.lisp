(in-package #:kakeibo/test/use-case/repository)

(coalton-toplevel
  (define (test-transaction-with-items-read)
    (to-test
     (result/t:run
      (do
       (trx <- (it/trx))
       (tid <- (>>= (valid trx)
                    (.< trans:lift trx:create)))
        (tid2 <- (>>= (valid trx)
                      (.< trans:lift trx:create)))
        (let itm1 = (itm:update-category "Item1" (it/itm tid)))
        (let itm2 = (itm:update-category "Item2" (it/itm tid)))
        (let itm3 = (itm:update-category "Item3" (it/itm tid)))
        (let itm-other = (itm:update-category "Item Other" (it/itm tid2)))
        (_ <- (>>= (valid itm1)
                   (.< result/t:some itm:create)))
        (_ <- (>>= (valid itm2)
                   (.< result/t:some itm:create)))
        (_ <- (>>= (valid itm3)
                   (.< result/t:some itm:create)))
        (_ <- (>>= (valid itm-other)
                   (.< result/t:some itm:create)))
        (rec <- (result/t:some (trx/itms:read tid)))
        (pure (and (== (trx:get-id (trx/itms:transaction rec))
                       tid)
                   (== (map itm:get-category (trx/itms:items rec))
                       (make-list "Item1" "Item2" "Item3"))))))))

  (define (test-transaction-with-items-search-case-1)
    (to-test
     (result/t:run
      (do
       (trx <- (it/trx))
       (trx1 <- (nest result/t:some result/t:hoist
                      (do
                       (d <- (date:date 2022 1 1))
                       (pure (trx:update-date d trx)))))
        (trx2 <- (nest result/t:some result/t:hoist
                       (do
                        (d <- (date:date 2023 1 1))
                        (pure (trx:update-date d trx)))))
        (trx3 <- (nest result/t:some result/t:hoist
                       (do
                        (d <- (date:date 2023 1 15))
                        (pure (trx:update-date d trx)))))
        (trx4 <- (nest result/t:some result/t:hoist
                       (do
                        (d <- (date:date 2023 10 1))
                        (pure (trx:update-date d trx)))))
        (_tid1 <- (>>= (valid trx1) (.< trans:lift trx:create)))
        (tid2 <- (>>= (valid trx2) (.< trans:lift trx:create)))
        (tid3 <- (>>= (valid trx3) (.< trans:lift trx:create)))
        (tid4 <- (>>= (valid trx4) (.< trans:lift trx:create)))
        (let itm1 = (itm:update-category "Item1" (it/itm tid2)))
        (let itm2 = (itm:update-category "Item2" (it/itm tid2)))
        (let itm3 = (itm:update-category "Item3" (it/itm tid2)))
        (let itm-other = (itm:update-category "Item Other" (it/itm tid4)))
        (_ <- (>>= (valid itm1) (.< result/t:some itm:create)))
        (_ <- (>>= (valid itm2) (.< result/t:some itm:create)))
        (_ <- (>>= (valid itm3) (.< result/t:some itm:create)))
        (_ <- (>>= (valid itm-other) (.< result/t:some itm:create)))
        (date1 <- (nest result/t:some result/t:hoist
                        (date:date 2023 1 1)))
        (date2 <- (nest result/t:some result/t:hoist
                        (date:date 2023 9 30)))
        (iter <- (nest trans:lift
                       (trx/itms:search
                        (trx/itms:SearchCondition (Some date1)
                                                  (Some date2)
                                                  None)
                        0
                        20)))
        (rec-trx3 <- (nest result/t:some result/t:hoist
                           (result:from-optional
                            (exception:Error "Empty Unit"))
                           (iter:next! iter)))
        (rec-trx2 <- (nest result/t:some result/t:hoist
                           (result:from-optional
                            (exception:Error "Empty Unit"))
                           (iter:next! iter)))
        (pure (and (== (trx:get-id (trx/itms:transaction rec-trx2))
                       tid2)
                   (== (trx:get-id (trx/itms:transaction rec-trx3))
                       tid3)
                   (== (map itm:get-category (trx/itms:items rec-trx2))
                       (make-list "Item1" "Item2" "Item3"))
                   (== (map itm:get-category (trx/itms:items rec-trx3))
                       (make-list))
                   (== (iter:count! iter)
                       0)))))))

  (define (test-transaction-with-items-amount/income)
    (to-test
     (result/t:run
      (do
       (trx <- (map (trx:update-type type:Income) (it/trx)))
       (tid <- (>>= (valid trx) (.< trans:lift trx:create)))
       (let itm1 = (itm:update-amount 200 (it/itm tid)))
       (let itm2 = (itm:update-amount 300 (it/itm tid)))
       (let itm3 = (itm:update-amount 500 (it/itm tid)))
       (_ <- (>>= (valid itm1) (.< result/t:some itm:create)))
       (_ <- (>>= (valid itm2) (.< result/t:some itm:create)))
       (_ <- (>>= (valid itm3) (.< result/t:some itm:create)))
       (rec <- (result/t:some (trx/itms:read tid)))
       (pure (and (== (trx/itms:amount rec)
                      (yen:Yen 1000))))))))

  (define (test-transaction-with-items-amount/outgo)
    (to-test
     (result/t:run
      (do
       (trx <- (map (trx:update-type type:Outgo) (it/trx)))
       (tid <- (>>= (valid trx) (.< trans:lift trx:create)))
       (let itm1 = (itm:update-amount 200 (it/itm tid)))
       (let itm2 = (itm:update-amount 300 (it/itm tid)))
       (let itm3 = (itm:update-amount 500 (it/itm tid)))
       (_ <- (>>= (valid itm1) (.< result/t:some itm:create)))
       (_ <- (>>= (valid itm2) (.< result/t:some itm:create)))
       (_ <- (>>= (valid itm3) (.< result/t:some itm:create)))
       (rec <- (result/t:some (trx/itms:read tid)))
       (pure (and (== (trx/itms:amount rec)
                      (yen:Yen -1000)))))))))
