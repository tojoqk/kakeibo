(defpackage #:kakeibo/test/repository/tree
  (:use #:coalton-testing)
  (:local-nicknames
   (#:tree #:kakeibo/repository/tree)
   (#:st #:coalton-library/monad/state)
   (#:entity #:kakeibo/test/entity/repository)
   (#:use-case #:kakeibo/test/use-case/repository)))

(in-package #:kakeibo/test/repository/tree)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(coalton-toplevel
  (define (run test)
    (match (st:run test
                   tree:init)
      ((Tuple _ b) b))))

(cl:defmacro add-test (symbol)
  `(define-test
       ,(cl:intern
         (cl:concatenate 'cl:string
                         "KAKEIBO/REPOSITORY/TREE-"
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
(add-test entity:test-item-different-ids)
(add-test entity:test-item-TransactionNotFoundOnCreate-error)
(add-test entity:test-item-NotFoundOnRead-error)
(add-test entity:test-item-NotFoundOnUpdate-error)
(add-test entity:test-item-TransactionNotFoundOnUpdate-error)
(add-test entity:test-item-NotFoundOnDelete-error)

(add-test use-case:test-transaction-with-items-read)
(add-test use-case:test-transaction-with-items-search-case-1)
(add-test use-case:test-transaction-with-items-amount/income)
(add-test use-case:test-transaction-with-items-amount/outgo)

(add-test use-case:test-item-with-transaction-read)
(add-test use-case:test-item-with-transaction-amount/income)
(add-test use-case:test-item-with-transaction-amount/outgo)
