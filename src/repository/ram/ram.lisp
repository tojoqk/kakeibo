(in-package #:kakeibo/repository/ram)

(coalton-toplevel
  (define-type Transaction
    (Transaction Integer (map:Map Integer (transaction:Transaction Integer))))

  (define-type Item
    (Item Integer (map:Map Integer (item:Item Integer Integer))))

  (define-type RAM
    (%RAM Transaction Item))

  (define init
    (%RAM (Transaction 1 map:empty)
          (Item 1 map:empty))))
