(defpackage #:kakeibo/test/entity/item
  (:use #:coalton-testing
        #:kakeibo/global/identity
        #:kakeibo/global/transformer/result)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:item #:kakeibo/entity/item)
   (#:tree #:coalton-library/ord-tree)
   (#:result #:coalton-library/result)))

(in-package #:kakeibo/test/entity/item)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(coalton-toplevel
  (define-type ItemId
    (UniqueId)
    (DuplicatedId))

  (define-instance (item:UniqueId Identity ItemId)
    (define (item:unique-id? x)
      (Identity
       (match x
         ((UniqueId) True)
         ((DuplicatedId) False)))))

  (define-instance (Eq ItemId)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (UniqueId) (UniqueId)) True)
        ((Tuple (DuplicatedId) (DuplicatedId)) True)
        (_ False))))

  (define-type TransactionId
    (TransactionId)
    (AnotherTransactionId))

  (define-instance (Eq TransactionId)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (TransactionId) (TransactionId)) True)
        ((Tuple (AnotherTransactionId) (AnotherTransactionId)) True)
        (_ False))))

  (define valid (.< runIdentity runResultT valid:valid)))

(coalton-toplevel
  (define item (item:Item UniqueId
                          TransactionId
                          "Cateogry"
                          (Some "Subcategory")
                          100
                          (Some "Note"))))

(define-test kakeibo/entity/item-get ()
  (is (== (item:get-id item) UniqueId))
  (is (== (item:get-transaction-id item) TransactionId))
  (is (== (item:get-category item) "Cateogry"))
  (is (== (item:get-subcategory item) (Some "Subcategory")))
  (is (== (item:get-amount item) 100))
  (is (== (item:get-note item) (Some "Note"))))

(define-test kakeibo/entity/item-update ()
  (is ((.> (item:update-id DuplicatedId)
           item:get-id
           (== DuplicatedId))
       item))
  (is ((.> (item:update-transaction-id AnotherTransactionId)
           item:get-transaction-id
           (== AnotherTransactionId))
       item))
  (is ((.> (item:update-category "")
           item:get-category
           (== ""))
       item))
  (is ((.> (item:update-subcategory None)
           item:get-subcategory
           (== None))
       item))
  (is ((.> (item:update-amount -1)
           item:get-amount
           (== -1))
       item))
  (is ((.> (item:update-note None)
           item:get-note
           (== None))
       item)))

(define-test kakeibo/entity/item-validation ()
  (is (== (match (valid item)
            ((Ok item_) (Some (valid:get item_)))
            (_ None))
          (Some item)))
  (is (nest result:ok? valid (item:update-subcategory None item)))
  (is (nest result:ok? valid (item:update-amount 1 item)))
  (is (nest result:ok? valid (item:update-amount 10 item)))
  (is (nest result:ok? valid (item:update-amount 2147483647 item)))
  (is (nest result:ok? valid (item:update-note None item)))

  (is (== (valid (item:update-id DuplicatedId item))
          (Err (item:Error (tree:make item:DuplicatedId)))))
  (is (== (valid (item:update-category "" item))
          (Err (item:Error (tree:make item:CategoryIsEmpty)))))
  (is (== (valid (item:update-subcategory (Some "") item))
          (Err (item:Error (tree:make item:SubcategoryIsEmpty)))))
  (is (== (valid (item:update-amount 0 item))
          (Err (item:Error (tree:make item:InvalidAmount)))))
  (is (== (valid (item:update-amount -10 item))
          (Err (item:Error (tree:make item:InvalidAmount)))))
  (is (== (valid (item:update-amount 2147483648 item))
          (Err (item:Error (tree:make item:InvalidAmount)))))
  (is (== (valid (item:update-note (Some "") item))
          (Err (item:Error (tree:make item:NoteIsEmpty)))))
  (is (nest (== (Err
                 (item:Error
                  (tree:make item:CategoryIsEmpty
                             item:NoteIsEmpty))))
            valid
            (item:update-category "")
            (item:update-note (Some ""))
            item))
  (is (nest (== (Err
                 (item:Error
                  (tree:make item:DuplicatedId
                             item:CategoryIsEmpty
                             item:SubcategoryIsEmpty
                             item:InvalidAmount
                             item:NoteIsEmpty))))
            valid
            (item:update-id DuplicatedId)
            (item:update-category "")
            (item:update-subcategory (Some ""))
            (item:update-amount 0)
            (item:update-note (Some ""))
            item)))
