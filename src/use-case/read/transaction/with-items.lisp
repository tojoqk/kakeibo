(cl:defpackage #:kakeibo/use-case/read/transaction/with-items
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:transaction #:kakeibo/entity/transaction)
   (#:item #:kakeibo/entity/item)
   (#:result/t #:kakeibo/global/result/trans)
   (#:exception #:kakeibo/global/exception))
  (:export
   #:Error #:NotFound

   #:Read))

(cl:in-package #:kakeibo/use-case/read/transaction/with-items)

(coalton-toplevel
  (define-type Error NotFound)
  (exception:define-exception-instance Error)

  (define-class (Monad :m => Read :m :id :iid (:m -> :id :iid))
    (read (:id -> result/t:ResultT Error :m
               (Tuple (transaction:Transaction :id)
                      (List (item:Item :iid :id)))))))
