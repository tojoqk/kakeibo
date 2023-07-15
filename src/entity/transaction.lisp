(cl:defpackage #:kakeibo/entity/transaction
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:tree #:coalton-library/ord-tree)
   (#:result #:coalton-library/result)
   (#:date #:kakeibo/global/date)
   (#:valid #:kakeibo/global/valid)
   (#:string #:coalton-library/string)
   (#:result/trans #:kakeibo/global/result/trans)
   (#:exception #:kakeibo/global/exception)
   (#:type #:kakeibo/entity/type))
  (:export
   #:Transaction
   #:get-id
   #:get-type
   #:get-date
   #:get-note
   #:update-type
   #:update-date
   #:update-note

   #:ValidateError
   #:ValidateErrorType
   #:InvalidDate
   #:NoteIsEmpty

   #:Creatable #:create
   #:Readable #:read
   #:ReadError #:NotFoundOnRead
   #:Updatable #:update
   #:UpdateError #:NotFoundOnUpdate
   #:Deletable #:delete
   #:DeleteError #:NotFoundOnDelete #:AssociatedItemsExist
   #:%set-id))

(cl:in-package #:kakeibo/entity/transaction)

(coalton-toplevel
  (define-type (Transaction :id)
    (%Transaction :id                    ; Id
                  type:Type              ; Type
                  date:Date              ; Date
                  (Optional String)      ; Note
                  ))

  (define (get-id (%Transaction id _ _ _)) id)
  (define (get-type (%Transaction _ type _ _)) type)
  (define (get-date (%Transaction _ _ date _)) date)
  (define (get-note (%Transaction _ _ _ note)) note)

  (define (update-type type (%Transaction id _ date note))
    (%Transaction id type date note))
  (define (update-date date (%Transaction id type _ note))
    (%Transaction id type date note))
  (define (update-note note (%Transaction id type date _))
    (%Transaction id type date note))

  (declare transaction (type:Type -> date:Date -> (Optional String) -> (Transaction Unit)))
  (define (transaction type date note)
    (%Transaction Unit type date note))

  (define-instance (Eq :id => Eq (Transaction :id))
    (define (== (%Transaction id1 type1 date1 note1)
                (%Transaction id2 type2 date2 note2))
      (and (== id1 id2)
           (== type1 type2)
           (== date1 date2)
           (== note1 note2))))

  (define-type ValidateError
    (ValidateError (tree:Tree ValidateErrorType)))
  (exception:define-exception-instance ValidateError)

  (define-instance (Eq ValidateError)
    (define (== (ValidateError x) (ValidateError y))
      (== x y)))

  (define-type ValidateErrorType
    (InvalidDate)
    (NoteIsEmpty))

  (define (error-type-code x)
    (match x
      ((InvalidDate) 0)
      ((NoteIsEmpty) 1)))

  (define-instance (Eq ValidateErrorType)
    (define (== x y)
      (== (error-type-code x)
          (error-type-code y))))

  (define-instance (Ord ValidateErrorType)
    (define (<=> x y)
      (<=> (error-type-code x)
           (error-type-code y))))

  (define-instance (valid:Validatable (Transaction :id) ValidateError)
    (define (valid:validate (%Transaction id _ date note))
      (let tree = (note-validation note))
      (if (== tree:empty tree)
          (pure Unit)
          (Err (ValidateError tree)))))

  (define (note-validation note)
    (match note
      ((Some note_)
       (if (== 0 (string:length note_))
           (tree:make NoteIsEmpty)
           tree:empty))
      (_ tree:empty)))

  (define-class (Monad :m => Creatable :m :id (:m -> :id))
    (create (valid:Valid (Transaction Unit) -> :m :id)))

  (define-type ReadError
    (NotFoundOnRead))
  (exception:define-exception-instance ReadError)

  (define-class (Monad :m => Readable :m :id (:m -> :id))
    (read (:id -> (result/trans:ResultT ReadError :m (Transaction :id)))))

  (define-type UpdateError
    (NotFoundOnUpdate))
  (exception:define-exception-instance UpdateError)

  (define-class (Monad :m => Updatable :m :id (:m -> :id))
    (update (valid:Valid (Transaction :id) -> result/trans:ResultT UpdateError :m Unit)))

  (define-type DeleteError
    (NotFoundOnDelete)
    (AssociatedItemsExist))
  (exception:define-exception-instance DeleteError)

  (define-class (Monad :m => Deletable :m :id (:m -> :id))
    (delete (:id -> (result/trans:ResultT DeleteError :m Unit))))

  (declare %set-id (Monad :m => :id -> Transaction Unit -> :m (Transaction :id)))
  (define (%set-id id (%Transaction (Unit) type date note))
    (pure (%Transaction id type date note))))
