(defpackage #:kakeibo/test/entity/date
  (:use #:coalton-testing
        #:kakeibo/global/identity
        #:kakeibo/global/transformer/result)
  (:local-nicknames
   (#:date #:kakeibo/entity/date)
   (#:valid #:kakeibo/global/valid)
   (#:result #:coalton-library/result)))

(in-package #:kakeibo/test/entity/date)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(define-test kakeibo/entity/date-month ()
  (map (fn (i)
         (is (match (the (Result String date:Month) (tryInto i))
               ((Ok m) (== (into m) i))
               (_ False))))
       (range 1 12))
  (is (result:err? (the (Result String date:Month) (tryInto 0))))
  (is (result:err? (the (Result String date:Month) (tryInto 13)))))

(coalton-toplevel
  (define valid (.< runIdentity runResultT valid:valid)))

(define-test kakeibo/entity/date-validation ()
  (map (fn (i)
         (is (match (tryInto i)
               ((Ok m)
                (nest result:err? valid (date:Date 2022 m 32)))
               (_ False))))
       (range 1 12))
  (is (nest result:err? valid (date:Date 2022 date:January 0)))
  (is (nest result:ok? valid (date:Date 2022 date:January 1)))
  (is (nest result:ok? valid (date:Date 2022 date:January 15)))
  (is (nest result:ok? valid (date:Date 2022 date:January 31)))
  (is (nest result:err? valid (date:Date 2022 date:January 32)))

  (is (nest result:ok? valid (date:Date 2022 date:February 28)))
  (is (nest result:err? valid (date:Date 2022 date:February 29)))

  (is (nest result:ok? valid (date:Date 4620 date:February 29)))
  (is (nest result:err? valid (date:Date 4620 date:February 30)))

  (is (nest result:ok? valid (date:Date 2000 date:February 29)))
  (is (nest result:err? valid (date:Date 2000 date:February 30)))

  (is (nest result:ok? valid (date:Date 2100 date:February 28)))
  (is (nest result:err? valid (date:Date 2100 date:February 29))))
