(cl:defpackage #:kakeibo/entity/item
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:valid #:kakeibo/global/valid)
   (#:tree #:coalton-library/ord-tree)
   (#:string #:coalton-library/string)
   (#:bounded #:coalton-library/math/bounded)
   (#:repo #:kakeibo/global/repository))
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

           #:ValidateError
           #:ValidateErrorType
           #:CategoryIsEmpty
           #:SubcategoryIsEmpty
           #:InvalidAmount
           #:NoteIsEmpty

           #:Creatable #:create!
           #:CreateError #:TransactionNotFoundOnCreate #:RepositoryErrorOnCreate
           #:Readable #:read!
           #:ReadError #:NotFoundOnRead #:RepositoryError #:RepositoryErrorOnRead
           #:Updatable #:update!
           #:UpdateError #:NotFoundOnUpdate #:TransactionNotFoundOnUpdate #:RepositoryErrorOnUpdate
           #:Deletable #:delete!
           #:DeleteError #:NotFoundOnDelete #:RepositoryErrorOnDelete
           #:%set-id))

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

  (define-instance (Eq ValidateError)
    (define (== (ValidateError x) (ValidateError y))
      (== x y)))

  (define-type ValidateErrorType
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

  (define-instance (Eq ValidateErrorType)
    (define (== x y)
      (== (error-type-code x)
          (error-type-code y))))

  (define-instance (Ord ValidateErrorType)
    (define (<=> x y)
      (<=> (error-type-code x)
           (error-type-code y))))

  (declare %validate (Item :id :tid -> Result ValidateError Unit))
  (define (%validate (%Item _ _ category subcategory amount note))
    (let tree = (mconcat (make-list
                          (validate-category category)
                          (validate-subcategory subcategory)
                          (validate-amount amount)
                          (validate-note note))))
    (if (== tree:empty tree)
        (Ok Unit)
        (Err (ValidateError tree))))

  (define-instance (valid:Validatable (Item :id :tid) ValidateError)
    (define valid:validate %validate))

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
    (if (or (<= amount 0)
            (< (into (the I32 bounded:maxbound)) amount))
        (tree:make InvalidAmount)
        tree:empty))

  (define (validate-note note)
    (match note
      ((Some note_)
       (if (== (string:length note_) 0)
           (tree:make NoteIsEmpty)
           tree:empty))
      (_ tree:empty)))

  (define-type ValidateError
    (ValidateError (tree:Tree ValidateErrorType)))

  (define-type (CreateError :e)
    (TransactionNotFoundOnCreate)
    (RepositoryErrorOnCreate :e))

  (define-class ((repo:Repository :r) => Creatable :r :e :id :tid (:r -> :e :id :tid))
    (create! (:r -> valid:Valid (Item Unit :tid) -> Result (CreateError :e) :id)))

  (define-type (ReadError :e)
    (NotFoundOnRead)
    (RepositoryErrorOnRead :e))

  (define-class ((repo:Repository :r) => Readable :r :e :id :tid (:r -> :e :id :tid))
    (read! (:r -> :id -> Result (ReadError :e) (Item :id :tid))))

  (define-type (UpdateError :e)
    (NotFoundOnUpdate)
    (TransactionNotFoundOnUpdate)
    (RepositoryErrorOnUpdate :e))

  (define-class ((repo:Repository :r) => Updatable :r :e :id :tid (:r -> :e :id :tid))
    (update! (:r -> valid:Valid (Item :id :tid) -> Result (UpdateError :e) Unit)))

  (define-type (DeleteError :e)
    (NotFoundOnDelete)
    (RepositoryErrorOnDelete :e))

  (define-class ((repo:Repository :r) => Deletable :r :e :id (:r -> :e :id))
    (delete! (:r -> :id -> Result (DeleteError :e) Unit)))

  (declare %set-id (:id -> Item Unit :tid -> (Item :id :tid)))
  (define (%set-id id (%Item (Unit) tid category subcategory amount note))
    (%Item id tid category subcategory amount note)))
