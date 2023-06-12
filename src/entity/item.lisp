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
           #:get-id #:get-transaction-id #:get-category #:get-subcategory #:get-amount #:get-note

           #:IdGenerator
           #:generate-id

           #:update-category
           #:update-subcategory
           #:update-amount
           #:update-note

           #:Error
           #:ErrorType
           #:CategoryIsEmpty #:SubcategoryIsEmpty #:InvalidAmount #:NoteIsEmpty))

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


  (declare item (IdGenerator :m :id => :tid -> String -> (Optional String) -> Integer -> (Optional String)
                              -> :m (Item :id :tid)))
  (define (item tid category subcategory amount note)
    (>>= (generate-id)
         (fn (id)
           (pure (%Item id tid category subcategory amount note)))))

  (define-class (Monad :m => IdGenerator :m :id)
    (generate-id (Unit -> :m :id)))

  (define-type Error
    (Error (tree:Tree ErrorType)))

  (define-instance (Eq Error)
    (define (== (Error x) (Error y))
      (== x y)))

  (define-type ErrorType
    (CategoryIsEmpty)
    (SubcategoryIsEmpty)
    (InvalidAmount)
    (NoteIsEmpty))

  (define-instance (Into ErrorType U8)
    (define (into x)
      (match x
        ((CategoryIsEmpty) 0)
        ((SubcategoryIsEmpty) 1)
        ((InvalidAmount) 2)
        ((NoteIsEmpty) 3))))

  (define-instance (Eq ErrorType)
    (define (== x y)
      (== (the U8 (into x)) (into y))))

  (define-instance (Ord ErrorType)
    (define (<=> x y)
      (<=> (the U8 (into x)) (into y))))

  (declare %validate (Monad :m => (Item :id :tid) -> (ResultT Error :m Unit)))
  (define (%validate (%Item _ _ category subcategory amount note))
    (do (tree <- (lift
                  (map mconcat
                       (sequence (make-list
                                  (validate-category category)
                                  (validate-subcategory subcategory)
                                  (validate-amount amount)
                                  (validate-note note))))))
        (if (== tree:empty tree)
            (pure Unit)
            (ResultT (pure (Err (Error tree)))))))

  (define-instance (Monad :m => valid:Validatable :m (Item :id :tid) Error)
    (define valid:validate %validate))

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
       (_ tree:empty)))))
