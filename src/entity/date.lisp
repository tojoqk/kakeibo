(cl:defpackage #:kakeibo/entity/date
  (:use #:coalton
        #:coalton-library/builtin
        #:coalton-library/classes
        #:coalton-library/functions
        #:kakeibo/global/transformer/result)
  (:local-nicknames
   (#:integral #:coalton-library/math/integral)
   (#:valid #:kakeibo/global/valid))
  (:export
   #:Date
   #:Month
   #:January #:February #:March
   #:April #:May #:June #:July #:August #:September
   #:October #:November #:December

   #:ErrorType
   #:InvalidDate))

(cl:in-package #:kakeibo/entity/date)

(coalton-toplevel
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

  (define-type Date (Date Integer       ; Year
                          Month         ; Month
                          Integer       ; Date
                          ))

  (define (leap? y)
    (and (== (integral:mod y 4) 0)
         (not (and (== (integral:mod y 100) 0)
                   (/= (integral:mod y 400) 0)))))

  (define-type ErrorType (InvalidDate))

  (define-instance (Monad :m => valid:Validatable :m Date ErrorType)
    (define (valid:validate (Date y m d))
      (let valid? =
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
               ((December)  (<= d 31)))))
      (ResultT
       (pure
        (if valid?
            (Ok Unit)
            (Err InvalidDate)))))))
