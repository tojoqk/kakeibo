(defpackage #:kakeibo/test/global/date
  (:use #:coalton-testing)
  (:local-nicknames
   (#:date #:kakeibo/global/date)
   (#:valid #:kakeibo/global/valid)
   (#:result #:coalton-library/result)))

(in-package #:kakeibo/test/global/date)

(coalton-fiasco-init #:kakeibo-test-fiasco)

(define-test kakeibo/global/date-validation ()
  (map (fn (i)
         (pipe i
               (fn (m) (date:make 2022 m 32))
               result:err?
               (== True)))
       (range 1 12))
  (is (pipe (date:make 2022 1 0)
            result:err?))
  (is (pipe (date:make 2022 1 1)
            result:ok?))
  (is (pipe (date:make 2022 1 15)
            result:ok?))
  (is (pipe (date:make 2022 1 31)
            result:ok?))
  (is (pipe (date:make 2022 1 32)
            result:err?))
  (is (pipe (date:make 2022 12 15)
            result:ok?))
  (is (pipe (date:make 2022 7 15)
            result:ok?))
  (is (pipe (date:make 2022 0 15)
            result:err?))
  (is (pipe (date:make 2022 13 15)
            result:err?))
  
  (is (pipe (date:make 2022 2 28)
            result:ok?))
  (is (pipe (date:make 2022 2 29)
            result:err?))

  (is (pipe (date:make 4620 2 29)
            result:ok?))
  (is (pipe (date:make 4620 2 30)
            result:err?))

  (is (pipe (date:make 2000 2 29)
            result:ok?))
  (is (pipe (date:make 2000 2 30)
            result:err?))

  (is (pipe (date:make 2100 2 28)
            result:ok?))
  (is (pipe (date:make 2100 2 29)
            result:err?)))
