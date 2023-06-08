(defpackage #:kakeibo/test/entity/date
  (:use #:coalton-testing)
  (:local-nicknames
   (#:date #:kakeibo/entity/date)
   (#:valid #:kakeibo/class/valid)
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

(define-test kakeibo/entity/date-validation ()
  (map (fn (i)
         (is (match (tryInto i)
               ((Ok m) (not (valid:valid? (date:Date 2022 m 32))))
               (_ False))))
       (range 1 12))

  (is (not (valid:valid? (date:Date 2022 date:January 0))))
  (is (valid:valid? (date:Date 2022 date:January 1)))
  (is (valid:valid? (date:Date 2022 date:January 15)))
  (is (valid:valid? (date:Date 2022 date:January 31)))
  (is (not (valid:valid? (date:Date 2022 date:January 32))))

  (is (valid:valid? (date:Date 2022 date:February 28)))
  (is (not (valid:valid? (date:Date 2022 date:February 29))))

  (is (valid:valid? (date:Date 4620 date:February 29)))
  (is (not (valid:valid? (date:Date 4620 date:February 30))))

  (is (valid:valid? (date:Date 2000 date:February 29)))
  (is (not (valid:valid? (date:Date 2000 date:February 30))))

  (is (valid:valid? (date:Date 2100 date:February 28)))
  (is (not (valid:valid? (date:Date 2100 date:February 29)))))
