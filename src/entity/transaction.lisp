(cl:defpackage #:kakeibo/entity/transaction
  (:use #:coalton
        #:coalton-library/classes
        #:kakeibo/global/identity
        #:kakeibo/global/transformer/result
        #:kakeibo/global/transformer/monad)
  (:local-nicknames
   (#:tree #:coalton-library/ord-tree)
   (#:result #:coalton-library/result)
   (#:date #:kakeibo/entity/date)
   (#:valid #:kakeibo/global/valid)
   (#:string #:coalton-library/string))
  (:export
   #:Transaction
   #:get-id
   #:get-type
   #:get-date
   #:get-note
   #:update-type
   #:update-date
   #:update-note

   #:Type
   #:Income
   #:Outgo

   #:ValidateError
   #:ValidateErrorType
   #:InvalidDate
   #:NoteIsEmpty))

(cl:in-package #:kakeibo/entity/transaction)

(coalton-toplevel
  (define-type (Transaction :id)
    (%Transaction :id                    ; Id
                  Type                   ; Type
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

  (declare transaction (Type -> date:Date -> (Optional String) -> (Transaction Unit)))
  (define (transaction type date note)
    (%Transaction Unit type date note))

  (define-instance (Eq :id => Eq (Transaction :id))
    (define (== (%Transaction id1 type1 date1 note1)
                (%Transaction id2 type2 date2 note2))
      (and (== id1 id2)
           (== type1 type2)
           (== date1 date2)
           (== note1 note2))))

  (define-type Type
    (Income)
    (Outgo))

  (define-instance (Eq Type)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Income) (Income)) True)
        ((Tuple (Outgo) (Outgo)) True)
        (_ False))))

  (define-type ValidateError
    (ValidateError (tree:Tree ValidateErrorType)))

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

  (define-instance (Monad :m => valid:Validatable :m (Transaction :id) ValidateError)
    (define (valid:validate (%Transaction id _ date note))
      (do (tree <- (lift
                    (map mconcat
                         (sequence
                          (make-list (date-validation date)
                                     (note-validation note))))))
          (if (== tree:empty tree)
              (pure Unit)
              (ResultT (pure (Err (ValidateError tree))))))))

  (define (date-validation date)
    (do (res <- (runResultT (valid:valid (the date:Date date))))
        (pure
         (match res
           ((Ok _) tree:empty)
           ((Err (date:InvalidDate))
            (tree:make InvalidDate))))))

  (define (note-validation note)
    (pure
     (match note
       ((Some note_)
        (if (== 0 (string:length note_))
            (tree:make NoteIsEmpty)
            tree:empty))
       (_ tree:empty))))

  (define-class (Monad :m => Creatable :m :id)
    (create (valid:Valid (Transaction Unit) -> :m :id)))

  (define-class (Monad :m => Readable :m :id)
    (read (:id -> :m (Transaction :id))))

  (define-class (Monad :m => Updatable :m :id)
    (update (valid:Valid (Transaction :id) -> :m Unit)))

  (define-class (Monad :m => Deletable :m :id)
    (%delete (:id -> :m Unit)))

  (define-class (Monad :m => AssociatedItemsExistence :m :id)
    (associated-items-exist? (:id -> (:m Boolean))))

  (define-type DeleteError (AssociatedItemsExist))


  (declare delete ((Deletable :m :id) (AssociatedItemsExistence :m :id) =>
                   :id -> ResultT DeleteError :m Unit))
  (define (delete id)
    (do (b <- (lift (associated-items-exist? id)))
        (if b
            (ResultT (pure (Err AssociatedItemsExist)))
            (lift (%delete id))))))
