(cl:defpackage #:kakeibo/global/exception
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:result #:coalton-library/result))
  (:export
   #:SomeException
   #:Exception
   #:to
   #:from
   #:define-exception-instance))

(cl:in-package #:kakeibo/global/exception)

(coalton-toplevel
  (repr :native cl:t)
  (define-type SomeException)

  (define-class (Exception :e)
    (to (:e -> SomeException))
    (from (SomeException -> (Optional :e))))

  (define-instance (Exception SomeException)
    (define (to e) e)
    (define (from e) (Some e)))

  (define-instance ((Exception :e) =>
                    Into
                    (Result :e :a)
                    (Result SomeException :a))
    (define (into m) (result:map-err to m))))

(cl:defmacro define-exception-instance (type)
  (cl:check-type type cl:symbol)
  (cl:let ((e (cl:gensym "EXCEPTION"))
           (se (cl:gensym "SOME")))
    `(define-instance (Exception ,type)
       (define (to ,e)
         (lisp SomeException (,e)
           (cl:cons ',type ,e)))
       (define (from ,se)
         (lisp (Optional ,type) (,se)
           (cl:if (cl:eq ',type (cl:car ,se))
                  (Some (cl:cdr ,se))
                  None))))))
