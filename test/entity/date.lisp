(defpackage #:kakeibo/test/entity/date
  (:use #:coalton-testing)
  (:local-nicknames
   (#:date #:kakeibo/entity/date)
   (#:valid #:kakeibo/global/valid)
   (#:result #:coalton-library/result)))

(in-package #:kakeibo/test/entity/date)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(define-test kakeibo/entity/date-month ()
  (map (fn (i)
         (pipe i
               (the (Integer -> Result String date:Month) tryInto)
               (map into)
               (== (Ok i))))
       (range 1 12))
  (is (pipe 0
            (the (Integer -> Result String date:Month) tryInto)
            result:err?))
  (is (pipe 13
            (the (Integer -> Result String date:Month) tryInto)
            result:err?)))

(define-test kakeibo/entity/date-validation ()
  (map (fn (i)
         (pipe i
               tryInto
               (map (fn (m) (date:make 2022 m 32)))
               (map result:err?)
               (== (Ok True))))
       (range 1 12))
  (is (pipe (date:make 2022 date:January 0)
            result:err?))
  (is (pipe (date:make 2022 date:January 1)
            result:ok?))
  (is (pipe (date:make 2022 date:January 15)
            result:ok?))
  (is (pipe (date:make 2022 date:January 31)
            result:ok?))
  (is (pipe (date:make 2022 date:January 32)
            result:err?))

  (is (pipe (date:make 2022 date:February 28)
            result:ok?))
  (is (pipe (date:make 2022 date:February 29)
            result:err?))

  (is (pipe (date:make 4620 date:February 29)
            result:ok?))
  (is (pipe (date:make 4620 date:February 30)
            result:err?))

  (is (pipe (date:make 2000 date:February 29)
            result:ok?))
  (is (pipe (date:make 2000 date:February 30)
            result:err?))

  (is (pipe (date:make 2100 date:February 28)
            result:ok?))
  (is (pipe (date:make 2100 date:February 29)
            result:err?)))
