(defpackage #:kakeibo/test/use-case/repository
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:date #:kakeibo/global/date)
   (#:trx #:kakeibo/entity/transaction)
   (#:itm #:kakeibo/entity/item)
   (#:trx/itms #:kakeibo/use-case/transaction-with-items)
   (#:valid #:kakeibo/global/valid)
   (#:result #:coalton-library/result)
   (#:result/t #:kakeibo/global/result/trans)
   (#:trans #:kakeibo/global/monad/trans)
   (#:exception #:kakeibo/global/exception)
   (#:iter #:coalton-library/iterator))
  (:export #:test-transaction-with-items-read
           #:test-transaction-with-items-search-case-1))

(in-package #:kakeibo/test/use-case/repository)

(coalton-toplevel
  (define (to-test m)
    (map (fn (res)
           (match res
             ((Ok b) b)
             ((Err _) False)))
         m))

  (define it/trx
    (trx:transaction trx:Income
                     (unwrap (date:make 2023 1 1))
                     (Some "Note")))

  (define (it/itm tid)
    (itm:item tid
              "Cateogry"
              (Some "Subcategory")
              100
              (Some "Note")))

  (define (test-transaction-with-items-read)
    (to-test
     (result/t:run
      (do
       (tid <- (trans:lift (trx:create (unwrap (valid:valid it/trx)))))
       (tid2 <- (trans:lift (trx:create (unwrap (valid:valid it/trx)))))
       (let itm1 = (itm:update-category "Item1" (it/itm tid)))
       (let itm2 = (itm:update-category "Item2" (it/itm tid)))
       (let itm3 = (itm:update-category "Item3" (it/itm tid)))
       (let itm-other = (itm:update-category "Item Other" (it/itm tid2)))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm1)))))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm2)))))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm3)))))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm-other)))))
       (rec <- (result/t:some (trx/itms:read tid)))
       (pure (and (== (trx:get-id (trx/itms:transaction rec))
                      tid)
                  (== (map itm:get-category (trx/itms:items rec))
                      (make-list "Item1" "Item2" "Item3"))))))))

  (define (test-transaction-with-items-search-case-1)
    (to-test
     (result/t:run
      (do
       (let trx1 = (trx:update-date (unwrap (date:make 2022 1 1)) it/trx))
       (let trx2 = (trx:update-date (unwrap (date:make 2023 1 1)) it/trx))
       (let trx3 = (trx:update-date (unwrap (date:make 2023 1 15)) it/trx))
       (let trx4 = (trx:update-date (unwrap (date:make 2023 10 1)) it/trx))
       (_tid1 <- (trans:lift (trx:create (unwrap (valid:valid trx1)))))
       (tid2 <- (trans:lift (trx:create (unwrap (valid:valid trx2)))))
       (tid3 <- (trans:lift (trx:create (unwrap (valid:valid trx3)))))
       (tid4 <- (trans:lift (trx:create (unwrap (valid:valid trx4)))))
       (let itm1 = (itm:update-category "Item1" (it/itm tid2)))
       (let itm2 = (itm:update-category "Item2" (it/itm tid2)))
       (let itm3 = (itm:update-category "Item3" (it/itm tid2)))
       (let itm-other = (itm:update-category "Item Other" (it/itm tid4)))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm1)))))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm2)))))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm3)))))
       (_ <- (result/t:some (itm:create (unwrap (valid:valid itm-other)))))
       (iter <- (trans:lift (trx/itms:search
                             (trx/itms:SearchCondition
                              (Some (unwrap (date:make 2023 1 1)))
                              (Some (unwrap (date:make 2023 9 30)))
                              None))))
       (let rec-trx3 = (unwrap (iter:next! iter)))
       (let rec-trx2 = (unwrap (iter:next! iter)))
       (pure (and (== (trx:get-id (trx/itms:transaction rec-trx2))
                      tid2)
                  (== (trx:get-id (trx/itms:transaction rec-trx3))
                      tid3)
                  (== (map itm:get-category (trx/itms:items rec-trx2))
                      (make-list "Item1" "Item2" "Item3"))
                  (== (map itm:get-category (trx/itms:items rec-trx3))
                      (make-list))
                  (== (iter:count! iter)
                      0))))))))
