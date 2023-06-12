(cl:defpackage #:kakeibo/entity/transaction
  (:use #:coalton
        #:coalton-library/classes
        #:kakeibo/global/identity
        #:kakeibo/global/transformer/result
        #:kakeibo/global/transformer/monad)
  (:shadow #:error)
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

   #:UniqueId #:unique-id?

   #:Error
   #:ErrorType
   #:DuplicatedId
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

  (define (transaction id type date note)
    (%Transaction id type date note))

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

  (define-class (Monad :m => UniqueId :m :id)
    (unique-id? (:id -> :m Boolean)))

  (define-type Error
    (Error (tree:Tree ErrorType)))

  (define-instance (Eq Error)
    (define (== (Error x) (Error y))
      (== x y)))

  (define-type ErrorType
    (DuplicatedId)
    (InvalidDate)
    (NoteIsEmpty))

  (define (error-type-code x)
    (match x
      ((DuplicatedId) 0)
      ((InvalidDate) 1)
      ((NoteIsEmpty) 2)))

  (define-instance (Eq ErrorType)
    (define (== x y)
      (== (error-type-code x)
          (error-type-code y))))

  (define-instance (Ord ErrorType)
    (define (<=> x y)
      (<=> (error-type-code x)
           (error-type-code y))))

  (define-instance (UniqueId :m :id => valid:Validatable :m (Transaction :id) Error)
    (define (valid:validate (%Transaction id _ date note))
      (do (tree <- (lift
                    (map mconcat
                         (sequence
                          (make-list (unique-id-validation id)
                                     (date-validation date)
                                     (note-validation note))))))
          (if (== tree:empty tree)
              (pure Unit)
              (ResultT (pure (Err (Error tree))))))))

  (define (unique-id-validation id)
    (do (b <- (unique-id? id))
        (pure
         (if b
             tree:empty
             (tree:make DuplicatedId)))))

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
       (_ tree:empty)))))
