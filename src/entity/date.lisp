(cl:defpackage #:kakeibo/entity/date
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:integral #:coalton-library/math/integral)
   (#:exception #:kakeibo/global/exception))
  (:export
   #:Date
   #:make
   #:year #:month #:day

   #:Month
   #:January #:February #:March
   #:April #:May #:June #:July #:August #:September
   #:October #:November #:December

   #:Error
   #:InvalidDate))

(cl:in-package #:kakeibo/entity/date)

(coalton-toplevel
  (define-type Date (%Date Integer      ; Year
                           Month        ; Month
                           Integer      ; Date
                           ))

  (declare make (Integer
                 -> Month
                 -> Integer
                 -> Result Error Date))
  (define (make y m d)
    (let date = (%Date y m d))
    (if (valid? date)
        (Ok date)
        (Err InvalidDate)))

  (define-type Error (InvalidDate))
  (exception:define-exception-instance Error)

  (define (year (%Date y _ _)) y)
  (define (month (%Date _ m _)) m)
  (define (day (%Date _ _ d)) d)

  (define-type Month
    January
    February
    March
    April
    May
    June
    July
    August
    September
    October
    November
    December)

  (define-instance (Into Month Integer)
    (define (into m)
      (match m
        ((January)   1)
        ((February)  2)
        ((March)     3)
        ((April)     4)
        ((May)       5)
        ((June)      6)
        ((July)      7)
        ((August)    8)
        ((September) 9)
        ((October)   10)
        ((November)  11)
        ((December)  12))))

  (define-instance (Eq Month)
    (define (== m1 m2)
      (== (the Integer (into m1))
          (into m2))))

  (define-instance (TryInto Integer Month)
    (define (tryInto m)
      (match m
        (1 (Ok January))
        (2 (Ok February))
        (3 (Ok March))
        (4 (Ok April))
        (5 (Ok May))
        (6 (Ok June))
        (7 (Ok July))
        (8 (Ok August))
        (9 (Ok September))
        (10 (Ok October))
        (11 (Ok November))
        (12 (Ok December))
        (_ (Err (<> (into m) " is not month number"))))))

  (define-instance (Eq Date)
    (define (== (%Date y1 m1 d1) (%Date y2 m2 d2))
      (and (== y1 y2)
           (== m1 m2)
           (== d1 d2))))

  (define (leap? y)
    (and (== (integral:mod y 4) 0)
         (not (and (== (integral:mod y 100) 0)
                   (/= (integral:mod y 400) 0)))))

  (define (valid? (%Date y m d))
    (and (<= 0 y)
         (<= 1 d)
         (match m
           ((January)   (<= d 31))
           ((February)  (if (leap? y)
                            (<= d 29)
                            (<= d 28)))
           ((March)     (<= d 31))
           ((April)     (<= d 30))
           ((May)       (<= d 31))
           ((June)      (<= d 30))
           ((July)      (<= d 31))
           ((August)    (<= d 31))
           ((September) (<= d 30))
           ((October)   (<= d 31))
           ((November)  (<= d 30))
           ((December)  (<= d 31))))))
