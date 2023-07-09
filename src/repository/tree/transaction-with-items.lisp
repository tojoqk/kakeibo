(in-package #:kakeibo/repository/tree)

(coalton-toplevel
  (define-instance (trx/itms:Read (st:ST Tree) TransactionId ItemId)
    (define (trx/itms:%read id)
      (do
       (opt <- (trans:lift (find-transaction id)))
       (match opt
         ((Some (Tuple trx itm-mp))
          (pure (Tuple trx
                       (iter:collect!
                        (map:values itm-mp)))))
         ((None)
          (result/t:hoist (Err trx/itms:NotFound)))))))

  (define (trx-match (trx/itms:SearchCondition start end note_) trx)
    (and (match start
           ((Some start) (<= start (transaction:get-date trx)))
           ((None) True))
         (match end
           ((Some end) (<= (transaction:get-date trx) end))
           ((None) True))
         (match note_
           ((Some note_)
            (match (transaction:get-note trx)
              ((Some note)
               (coalton-library/string:substring? note note_))
              ((None) False)))
           ((None) True))))

  (declare iter-drop! (UFix -> iter:Iterator :a -> iter:Iterator :a))
  (define (iter-drop! count iter)
    (lisp Unit (count iter)
      (cl:dotimes (i count Unit)
        (iter:next! iter)))
    iter)

  (define-instance (trx/itms:Search (st:St Tree) TransactionId ItemId)
    (define (trx/itms:%search cnd offset limit)
      (>>= st:get
           (fn ((%Tree _ _ trx-mp))
             (pipe (map:values trx-mp)
                   (iter:filter! (.< (trx-match cnd) fst))
                   (iter-drop! offset)
                   (iter:take! limit)
                   (map (map-snd (.< iter:collect! map:values)))
                   pure))))))
