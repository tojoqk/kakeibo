(cl:defpackage #:kakeibo/entity/item
  (:use #:coalton
        #:coalton-library/classes
        #:kakeibo/class/writer)
  (:shadow #:error)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:tree #:coalton-library/ord-tree)
   (#:string #:coalton-library/string)
   (#:bounded #:coalton-library/math/bounded))
  (:export #:Item
           #:get-id #:get-transaction-id #:get-category #:get-amount #:get-note
           #:update-category #:update-subcategory #:update-amount #:update-note))

(cl:in-package #:kakeibo/entity/item)

(coalton-toplevel
  (define-type (Item :id)
    (Item :id                           ; ID
          :id                           ; Transaction-ID
          String                        ; Category
          (Optional String)             ; Subcategory
          Integer                       ; Amount
          (Optional String)             ; Note
          ))

  (define (get-id (Item id _ _ _ _ _)) id)
  (define (get-transaction-id (Item _ tid _ _ _ _)) tid)
  (define (get-category (Item _ _ category _ _ _)) category)
  (define (get-subcategory (Item _ _ _ subcategory _ _)) subcategory)
  (define (get-amount (Item _ _ _ _ amount _)) amount)
  (define (get-note (Item _ _ _ _ _ note)) note)

  (define (update-category category (Item id tid _ subcategory amount note))
    (Item id tid category subcategory amount note))
  (define (update-subcategory subcategory (Item id tid category _ amount note))
    (Item id tid category subcategory amount note))
  (define (update-amount amount (Item id tid category subcategory _ note))
    (Item id tid category subcategory amount note))
  (define (update-note note (Item id tid category subcategory amount _))
    (Item id tid category subcategory amount note))

  (define-class (UniqueId :id)
    (unique-id!? (:id -> Boolean)))

  (define-type Error
    (Error (tree:Tree ErrorType)))

  (define-type ErrorType
    (NonUniqueId)
    (CategoryIsEmpty)
    (SubcategoryIsEmpty)
    (InvalidAmount))

  (define-instance (Into ErrorType U8)
    (define (into x)
      (match x
        ((NonUniqueId) 0)
        ((CategoryIsEmpty) 1)
        ((SubcategoryIsEmpty) 2)
        ((InvalidAmount) 3))))

  (define-instance (Eq ErrorType)
    (define (== x y)
      (== (the U8 (into x)) (into y))))

  (define-instance (Ord ErrorType)
    (define (<=> x y)
      (<=> (the U8 (into x)) (into y))))

  (define-instance (UniqueId :id => valid:Validatable! (Item :id) Error)
    (define (valid:validate! (Item id tid category subcategory amount note))
      (let e =
        (mconcat
         (make-list
          (validate-uniquie-id! id)
          (validate-category category)
          (validate-subcategory subcategory)
          (validate-amount amount)
          (validate-note note))))
      (if (== tree:empty e)
          (Ok Unit)
          (Err (Error e)))))

  (define (validate-uniquie-id! id)
    (if (unique-id!? id)
        tree:empty
        (tree:make NonUniqueId)))

  (define (validate-category category)
    (if (== (string:length category) 0)
        (tree:make CategoryIsEmpty)
        tree:empty))

  (define (validate-subcategory subcategory)
    (match subcategory
      ((Some subcategory_)
       (if (== (string:length subcategory_) 0)
           (tree:make SubcategoryIsEmpty)
           tree:empty))
      (_ tree:empty)))

  (define (validate-amount amount)
    (if (and (< 0 amount)
             (< amount (into (the I32 bounded:maxbound))))
        (tree:make InvalidAmount)
        tree:empty))

  (define (validate-note note)
    (match note
      ((Some note_)
       (if (== (string:length note_) 0)
           (tree:make SubcategoryIsEmpty)
           tree:empty))
      (_ tree:empty)))
  )
