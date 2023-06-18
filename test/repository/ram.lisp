(defpackage #:kakeibo/test/repository/ram
  (:use #:coalton-testing)
  (:local-nicknames
   (#:ram #:kakeibo/repository/ram)
   (#:st #:coalton-library/monad/state)
   (#:entity #:kakeibo/test/entity/repository)))

(in-package #:kakeibo/test/repository/ram)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(coalton-toplevel
  (define (run test)
    (match (st:run test
                   ram:init)
      ((Tuple _ b) b))))

(cl:defmacro add-test (symbol)
  `(define-test
       ,(cl:intern
         (cl:concatenate 'cl:string
                         "KAKEIBO/REPOSITORY/RAM-"
                         (cl:symbol-name symbol)))
       ()
     (is (run (,symbol)))))

(add-test entity:test-transaction-create)
(add-test entity:test-transaction-read)
(add-test entity:test-transaction-update)
(add-test entity:test-transaction-delete)
(add-test entity:test-transaction-different-ids)
(add-test entity:test-transaction-NotFoundOnRead-error)
(add-test entity:test-transaction-NotFoundOnUpdate-error)
(add-test entity:test-transaction-NotFoundOnDelete-error)
(add-test entity:test-transaction-AssociatedItemsExist-error)

(add-test entity:test-item-create)
(add-test entity:test-item-read)
(add-test entity:test-item-update)
(add-test entity:test-item-delete)
(add-test entity:test-item-TransactionNotFoundOnCreate-error)
(add-test entity:test-item-NotFoundOnRead-error)
(add-test entity:test-item-NotFoundOnUpdate-error)
(add-test entity:test-item-TransactionNotFoundOnUpdate-error)
(add-test entity:test-item-NotFoundOnDelete-error)
