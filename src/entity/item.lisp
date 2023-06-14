(cl:defpackage #:kakeibo/entity/item
  (:use #:coalton
        #:coalton-library/classes
        #:kakeibo/global/transformer/result
        #:kakeibo/global/transformer/monad)
  (:shadow #:error)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:tree #:coalton-library/ord-tree)
   (#:string #:coalton-library/string)
   (#:bounded #:coalton-library/math/bounded))
  (:export #:Item
           #:get-id
           #:get-transaction-id
           #:get-category
           #:get-subcategory
           #:get-amount
           #:get-note
           #:update-category
           #:update-subcategory
           #:update-amount
           #:update-note

           #:TransactionIdExistence
           #:transaction-id-exists?

           #:Error
           #:ErrorType
           #:TransactionIdDoesNotExist
           #:CategoryIsEmpty
           #:SubcategoryIsEmpty
           #:InvalidAmount
           #:NoteIsEmpty))

(cl:in-package #:kakeibo/entity/item)

(coalton-toplevel
  (define-type (Item :id :transaction-id)
    (%Item :id                           ; ID
           :transaction-id               ; Transaction-ID
           String                        ; Category
           (Optional String)             ; Subcategory
           Integer                       ; Amount
           (Optional String)             ; Note
           ))

  (define-instance ((Eq :id) (Eq :tid) => Eq (Item :id :tid))
    (define (== (%Item id1 tid1 category1 subcateogry1 amount1 note1)
                (%Item id2 tid2 category2 subcateogry2 amount2 note2))
      (and (== id1 id2)
           (== tid1 tid2)
           (== category1 category2)
           (== category1 category2)
           (== subcateogry1 subcateogry2)
           (== amount1 amount2)
           (== note1 note2))))

  (define (get-id (%Item id _ _ _ _ _)) id)
  (define (get-transaction-id (%Item _ tid _ _ _ _)) tid)
  (define (get-category (%Item _ _ category _ _ _)) category)
  (define (get-subcategory (%Item _ _ _ subcategory _ _)) subcategory)
  (define (get-amount (%Item _ _ _ _ amount _)) amount)
  (define (get-note (%Item _ _ _ _ _ note)) note)

  (define (update-category category (%Item id tid _ subcategory amount note))
    (%Item id tid category subcategory amount note))
  (define (update-subcategory subcategory (%Item id tid category _ amount note))
    (%Item id tid category subcategory amount note))
  (define (update-amount amount (%Item id tid category subcategory _ note))
    (%Item id tid category subcategory amount note))
  (define (update-note note (%Item id tid category subcategory amount _))
    (%Item id tid category subcategory amount note))

  (declare item (:tid -> String -> (Optional String) -> Integer -> (Optional String)
                      -> (Item Unit :tid)))
  (define (item tid category subcategory amount note)
    (%Item Unit tid category subcategory amount note))

  (define-type Error
    (Error (tree:Tree ErrorType)))

  (define-instance (Eq Error)
    (define (== (Error x) (Error y))
      (== x y)))

  (define-type ErrorType
    (TransactionIdDoesNotExist)
    (CategoryIsEmpty)
    (SubcategoryIsEmpty)
    (InvalidAmount)
    (NoteIsEmpty))

  (define (error-type-code t)
    (match t
      ((TransactionIdDoesNotExist) 0)
      ((CategoryIsEmpty) 1)
      ((SubcategoryIsEmpty) 2)
      ((InvalidAmount) 3)
      ((NoteIsEmpty) 4)))

  (define-instance (Eq ErrorType)
    (define (== x y)
      (== (error-type-code x)
          (error-type-code y))))

  (define-instance (Ord ErrorType)
    (define (<=> x y)
      (<=> (error-type-code x)
           (error-type-code y))))

  (define-class (Monad :m => TransactionIdExistence :m :tid)
    (transaction-id-exists? (:tid -> :m Boolean)))

  (declare %validate (TransactionIdExistence :m :tid => (Item :id :tid) -> (ResultT Error :m Unit)))
  (define (%validate (%Item _ tid category subcategory amount note))
    (do (tree <- (lift
                  (map mconcat
                       (sequence (make-list
                                  (validate-transaction-id tid)
                                  (validate-category category)
                                  (validate-subcategory subcategory)
                                  (validate-amount amount)
                                  (validate-note note))))))
        (if (== tree:empty tree)
            (pure Unit)
            (ResultT (pure (Err (Error tree)))))))

  (define-instance (TransactionIdExistence :m :tid => valid:Validatable :m (Item :id :tid) Error)
    (define valid:validate %validate))

  (define (validate-transaction-id tid)
    (do (b <- (transaction-id-exists? tid))
        (pure
         (if b
             tree:empty
             (tree:make TransactionIdDoesNotExist)))))

  (define (validate-category category)
    (pure (if (== (string:length category) 0)
              (tree:make CategoryIsEmpty)
              tree:empty)))

  (define (validate-subcategory subcategory)
    (pure
     (match subcategory
       ((Some subcategory_)
        (if (== (string:length subcategory_) 0)
            (tree:make SubcategoryIsEmpty)
            tree:empty))
       (_ tree:empty))))

  (define (validate-amount amount)
    (pure
     (if (or (<= amount 0)
             (< (into (the I32 bounded:maxbound)) amount))
         (tree:make InvalidAmount)
         tree:empty)))

  (define (validate-note note)
    (pure
     (match note
       ((Some note_)
        (if (== (string:length note_) 0)
            (tree:make NoteIsEmpty)
            tree:empty))
       (_ tree:empty))))

  (define-class (Monad :m => Creatable :m :id :tid)
    (create (valid:Valid (Item Unit :tid) -> :m :id)))

  (define-class (Monad :m => Readable :m :id :tid)
    (read (:id -> :m (Item :id :tid))))

  (define-class (Monad :m => Updatable :m :id :tid)
    (udpate (valid:Valid (Item :id :tid) -> :m Unit)))

  (define-class (Monad :m => Deletable :m :id)
    (delete (:id -> :m Unit))))
