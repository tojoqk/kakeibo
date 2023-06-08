(defpackage #:kakeibo/test/entity/item
  (:use #:coalton-testing)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:item #:kakeibo/entity/item)
   (#:tree #:coalton-library/ord-tree)))

(in-package #:kakeibo/test/entity/item)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(coalton-toplevel
  (define-type ItemId
    (UniqueId)
    (DuplicatedId))

  (define-instance (item:UniqueId ItemId)
    (define (item:unique-id!? x)
      (match x
        ((UniqueId) True)
        ((DuplicatedId) False))))

  (define-instance (Eq ItemId)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (UniqueId) (UniqueId)) True)
        ((Tuple (DuplicatedId) (DuplicatedId)) True)
        (_ False))))

  (define-type TransactionId (TransactionId))
  (define-instance (Eq TransactionId)
    (define (== (TransactionId) (TransactionId)) True)))

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

(define-test kakeibo/entity/item-validation ()
  (is (== (match (valid:valid! item)
            ((Ok item_) (Some (valid:get item_)))
            (_ None))
          (Some item)))
  (is (valid:valid!? (item:update-subcategory None item)))
  (is (valid:valid!? (item:update-amount 1 item)))
  (is (valid:valid!? (item:update-amount 10 item)))
  (is (valid:valid!? (item:update-amount 2147483647 item)))
  (is (valid:valid!? (item:update-note None item)))

  (is (== (valid:valid! (item:update-id DuplicatedId item))
          (Err (item:Error (tree:make item:DuplicatedId)))))
  (is (== (valid:valid! (item:update-category "" item))
          (Err (item:Error (tree:make item:CategoryIsEmpty)))))
  (is (== (valid:valid! (item:update-subcategory (Some "") item))
          (Err (item:Error (tree:make item:SubcategoryIsEmpty)))))
  (is (== (valid:valid! (item:update-amount 0 item))
          (Err (item:Error (tree:make item:InvalidAmount)))))
  (is (== (valid:valid! (item:update-amount -10 item))
          (Err (item:Error (tree:make item:InvalidAmount)))))
  (is (== (valid:valid! (item:update-amount 2147483648 item))
          (Err (item:Error (tree:make item:InvalidAmount)))))
  (is (== (valid:valid! (item:update-note (Some "") item))
          (Err (item:Error (tree:make item:NoteIsEmpty)))))
  (is ((.> (item:update-category "")
           (item:update-note (Some ""))
           valid:valid!
           (== (Err (item:Error (tree:make item:CategoryIsEmpty
                                           item:NoteIsEmpty)))))
       item))
  (is ((.> (item:update-id DuplicatedId)
           (item:update-category "")
           (item:update-subcategory (Some ""))
           (item:update-amount 0)
           (item:update-note (Some ""))
           valid:valid!
           (== (Err (item:Error (tree:make item:DuplicatedId
                                           item:CategoryIsEmpty
                                           item:SubcategoryIsEmpty
                                           item:InvalidAmount
                                           item:NoteIsEmpty)))))
       item)))
