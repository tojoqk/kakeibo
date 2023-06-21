(cl:defpackage #:kakeibo/use-case/search/transactions/with-items
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error
           #:and)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:transaction #:kakeibo/entity/transaction)
   (#:item #:kakeibo/entity/item)
   (#:result/t #:kakeibo/global/result/trans)
   (#:exception #:kakeibo/global/exception)
   (#:date #:kakeibo/global/date))
  (:export
   #:Condition
   #:get-start-date
   #:get-end-date
   #:get-note

   #:Search))

(cl:in-package #:kakeibo/use-case/search/transactions/with-items)

(coalton-toplevel
  (define-type Condition
    (Condition
     (Optional date:Date)               ; start date
     (Optional date:Date)               ; end date
     (Optional String)                  ; note
     ))

  (define (get-start-date (Condition sd _ _)) sd)
  (define (get-end-date (Condition _ ed _)) ed)
  (define (get-note (Condition _ _ note)) note)

  (define (set-start-date sd (Condition _sd ed note))
    (Condition sd ed note))
  (define (set-end-date ed (Condition sd _ed note))
    (Condition sd ed note))
  (define (set-note note (Condition sd ed _note))
    (Condition sd ed note))

  (define-class (Monad :m => Search :m :id :iid (:m -> :id :iid))
    (search (Condition
             -> :m (iter:Iterator
                    (Tuple (transaction:Transaction :id)
                           (List (item:Item :iid :id))))))))
