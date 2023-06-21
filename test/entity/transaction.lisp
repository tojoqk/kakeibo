(defpackage #:kakeibo/test/entity/transaction
  (:use #:coalton-testing)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:date #:kakeibo/global/date)
   (#:transaction #:kakeibo/entity/transaction)
   (#:tree #:coalton-library/ord-tree)
   (#:result #:coalton-library/result)))

(in-package #:kakeibo/test/entity/transaction)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(coalton-toplevel
  (define it
    (transaction:transaction transaction:Income
                             (unwrap (date:make 2023 1 1))
                             (Some "Note"))))

(define-test kakeibo/entity/transaction-get ()
  (is (pipe it
            transaction:get-id
            (== Unit)))
  (is (pipe it
            transaction:get-type
            (== transaction:Income)))
  (is (pipe it
            transaction:get-date
            (== (unwrap (date:make 2023 1 1)))))
  (is (pipe it
            transaction:get-note
            (== (Some "Note")))))

(define-test kakeibo/entity/transaction-update ()
  (is (pipe it
            (transaction:update-type transaction:Outgo)
            transaction:get-type
            (== transaction:Outgo)))
  (is (pipe it
            (transaction:update-date
             (unwrap (date:make 2023 1 2)))
            transaction:get-date
            (== (unwrap (date:make 2023 1 2)))))
  (is (pipe it
            (transaction:update-note (Some "Note2"))
            transaction:get-note
            (== (Some "Note2")))))

(define-test kakeibo/entity/transaction-validation ()
  (is (pipe it
            valid:valid as-optional (map valid:get)
            (== (Some it))))
  (is (pipe it valid:valid result:ok?))
  (is (pipe it
            (transaction:update-type transaction:OutGo)
            valid:valid result:ok?))
  (is (pipe it
            (transaction:update-note None)
            valid:valid result:ok?))

  (is (pipe it
            (transaction:update-note (Some ""))
            valid:valid
            (== (Err (transaction:ValidateError
                      (tree:make transaction:NoteIsEmpty))))))
  (is (pipe it
            (transaction:update-note (Some ""))
            valid:valid
            (== (Err (transaction:ValidateError
                      (tree:make transaction:NoteIsEmpty)))))))
