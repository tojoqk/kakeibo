(defpackage #:kakeibo/test/entity/transaction
  (:use #:coalton-testing
        #:kakeibo/global/identity
        #:kakeibo/global/transformer/result)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:date #:kakeibo/entity/date)
   (#:transaction #:kakeibo/entity/transaction)
   (#:tree #:coalton-library/ord-tree)
   (#:result #:coalton-library/result)))

(in-package #:kakeibo/test/entity/transaction)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(coalton-toplevel
  (define-type TransactionId
    (UniqueId)
    (DuplicatedId))

  (define-instance (transaction:UniqueId Identity TransactionId)
    (define (transaction:unique-id? x)
      (Identity
       (match x
         ((UniqueId) True)
         ((DuplicatedId) False)))))

  (define-instance (Eq TransactionId)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (UniqueId) (UniqueId)) True)
        ((Tuple (DuplicatedId) (DuplicatedId)) True)
        (_ False))))

  (define valid (.< runIdentity runResultT valid:valid)))

(coalton-toplevel
  (define it
    (transaction:transaction UniqueId
                             transaction:Income
                             (date:Date 2023 date:January 1)
                             (Some "Note"))))

(define-test kakeibo/entity/transaction-get ()
  (is (pipe it
            transaction:get-id
            (== UniqueId)))
  (is (pipe it
            transaction:get-type
            (== transaction:Income)))
  (is (pipe it
            transaction:get-date
            (== (date:Date 2023 date:January 1))))
  (is (pipe it
            transaction:get-note
            (== (Some "Note")))))

(define-test kakeibo/entity/transaction-update ()
  (is (pipe it
            (transaction:update-id DuplicatedId)
            transaction:get-id
            (== DuplicatedId)))
  (is (pipe it
            (transaction:update-type transaction:Outgo)
            transaction:get-type
            (== transaction:Outgo)))
  (is (pipe it
            (transaction:update-date (date:Date 2023 date:January 2))
            transaction:get-date
            (== (date:Date 2023 date:January 2))))
  (is (pipe it
            (transaction:update-note (Some "Note2"))
            transaction:get-note
            (== (Some "Note2")))))

(define-test kakeibo/entity/transaction-validation ()
  (is (pipe it
            valid as-optional (map valid:get)
            (== (Some it))))
  (is (pipe it valid result:ok?))
  (is (pipe it
            (transaction:update-type transaction:OutGo)
            valid result:ok?))
  (is (pipe it
            (transaction:update-note None)
            valid result:ok?))

  (is (pipe it
            (transaction:update-id DuplicatedId)
            valid
            (== (Err (transaction:Error
                      (tree:make transaction:DuplicatedId))))))
  (is (pipe it
            (transaction:update-date
             (date:Date 2023 date:January 32))
            valid
            (== (Err (transaction:Error
                      (tree:make transaction:InvalidDate))))))
  (is (pipe it
            (transaction:update-note (Some ""))
            valid
            (== (Err (transaction:Error
                      (tree:make transaction:NoteIsEmpty))))))
  (is (pipe it
            (transaction:update-id DuplicatedId)
            (transaction:update-date
             (date:Date 2023 date:January 32))
            (transaction:update-note (Some ""))
            valid
            (== (Err (transaction:Error
                      (tree:make transaction:DuplicatedId
                                 transaction:InvalidDate
                                 transaction:NoteIsEmpty)))))))
