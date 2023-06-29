(cl:defpackage #:kakeibo/global/exception
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:some #:error)
  (:local-nicknames
   (#:prelude #:coalton-prelude)
   (#:result #:coalton-library/result))
  (:export
   #:Some
   #:Exception
   #:to
   #:from
   #:Error
   #:define-exception-instance))

(cl:in-package #:kakeibo/global/exception)

(coalton-toplevel
  (repr :native cl:t)
  (define-type Some)

  (define-class (Exception :e)
    (to (:e -> Some))
    (from (Some -> (Optional :e))))

  (define-instance (Exception Some)
    (define (to e) e)
    (define (from e) (prelude:Some e)))
  )


(cl:defmacro define-exception-instance (type)
  (cl:check-type type cl:symbol)
  (cl:let ((e (cl:gensym "EXCEPTION"))
           (se (cl:gensym "SOME")))
    `(define-instance (Exception ,type)
       (define (to ,e)
         (lisp Some (,e)
           (cl:cons ',type ,e)))
       (define (from ,se)
         (lisp (Optional ,type) (,se)
           (cl:if (cl:eq ',type (cl:car ,se))
                  (prelude:Some (cl:cdr ,se))
                  None))))))

(coalton-toplevel
  (define-type Error (Error String))
  (define-exception-instance Error))
