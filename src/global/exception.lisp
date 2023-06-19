(cl:defpackage #:kakeibo/global/exception
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:result/trans #:kakeibo/global/result/trans))
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

  (define-instance ((Exception :e)
                    (Bifunctor :m) =>
                    Into
                    (:m :a :e)
                    (:m :a SomeException))
    (define (into m) (map-snd to m)))

  (define-instance ((Exception :e) (Monad :m) =>
                    Into
                    (result/trans:T :e :m :a)
                    (result/trans:T SomeException :m :a))
    (define (into m)
      (result/trans:T (map (fn (x)
                             (match x
                               ((Ok x) (Ok x))
                               ((Err x) (Err (to x)))))
                           (result/trans:run m))))))

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
