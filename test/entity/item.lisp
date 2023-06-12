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
  (define-type ItemId (ItemId))

  (define-instance (item:IdGenerator Identity ItemId)
    (define (item:generate-id) (pure ItemId)))

  (define-instance (Eq ItemId)
    (define (== (ItemId) (ItemId)) True))

  (define-type TransactionId (TransactionId))

  (define-instance (Eq TransactionId)
    (define (== (TransactionId) (TransactionId)) True))

  (define valid (.< runIdentity runResultT valid:valid)))

(coalton-toplevel
  (declare it (item:Item ItemId TransactionId))
  (define it
    (runIdentity
     (item:Item TransactionId
                "Cateogry"
                (Some "Subcategory")
                100
                (Some "Note")))))

(define-test kakeibo/entity/item-get ()
  (is (pipe it
            item:get-id
            (== ItemId)))
  (is (pipe it
            item:get-transaction-id
            (== TransactionId)))
  (is (pipe it
            item:get-category
            (== "Cateogry")))
  (is (pipe it
            item:get-subcategory
            (== (Some "Subcategory"))))
  (is (pipe it
            item:get-amount
            (== 100)))
  (is (pipe it
            item:get-note
            (== (Some "Note")))))

(define-test kakeibo/entity/item-update ()
  (is (pipe it
            (item:update-category "")
            item:get-category
            (== "")))
  (is (pipe it
            (item:update-subcategory None)
            item:get-subcategory
            (== None)))
  (is (pipe it
            (item:update-amount -1)
            item:get-amount
            (== -1)))
  (is (pipe it
            (item:update-note None)
            item:get-note
            (== None))))

(define-test kakeibo/entity/item-validation ()
  (is (pipe it
            valid
            as-optional
            (map valid:get)
            (== (Some it))))
  (is (pipe it
            (item:update-subcategory None)
            valid result:ok?))
  (is (pipe it
            (item:update-amount 1)
            valid result:ok?))
  (is (pipe it
            (item:update-amount 10)
            valid result:ok?))
  (is (pipe it
            (item:update-amount 2147483647)
            valid result:ok?))
  (is (pipe it
            (item:update-note None)
            valid result:ok?))

  (is (pipe it
            (item:update-category "")
            valid
            (== (Err (item:Error (tree:make item:CategoryIsEmpty))))))
  (is (pipe it
            (item:update-subcategory (Some ""))
            valid
            (== (Err (item:Error (tree:make item:SubcategoryIsEmpty))))))
  (is (pipe it
            (item:update-amount 0)
            valid
            (== (Err (item:Error (tree:make item:InvalidAmount))))))
  (is (pipe it
            (item:update-amount -10)
            valid
            (== (Err (item:Error (tree:make item:InvalidAmount))))))
  (is (pipe it
            (item:update-amount 2147483648)
            valid
            (== (Err (item:Error (tree:make item:InvalidAmount))))))
  (is (pipe it
            (item:update-note (Some ""))
            valid
            (== (Err (item:Error (tree:make item:NoteIsEmpty))))))
  (is (pipe it
            (item:update-category "")
            (item:update-note (Some ""))
            valid
            (== (Err
                 (item:Error
                  (tree:make item:CategoryIsEmpty
                             item:NoteIsEmpty))))))
  (is (pipe it
            (item:update-category "")
            (item:update-subcategory (Some ""))
            (item:update-amount 0)
            (item:update-note (Some ""))
            valid
            (== (Err
                 (item:Error
                  (tree:make item:CategoryIsEmpty
                             item:SubcategoryIsEmpty
                             item:InvalidAmount
                             item:NoteIsEmpty)))))))
