(in-package #:kakeibo/test/use-case/repository)

(coalton-toplevel
  (define (to-test m)
    (map (fn (res)
           (match res
             ((Ok b) b)
             ((Err _) False)))
         m))

  (define (it/trx)
    (nest result/t:some result/t:hoist
          (do (d <- (date:date 2023 1 1))
              (pure (trx:transaction type:Income
                                     d
                                     (Some "Note"))))))

  (define (it/itm tid)
    (itm:item tid
              "Cateogry"
              (Some "Subcategory")
              100
              (Some "Note")))

  (define (valid x)
    (nest result/t:some
          result/t:ResultT
          pure valid:valid x)))
