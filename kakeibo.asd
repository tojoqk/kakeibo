(asdf:defsystem #:kakeibo
  :description "Managing my personal household accounts"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "global/exception")
               (:file "global/result")
               (:file "global/monad/trans")
               (:file "global/result/trans")
               (:file "global/valid")
               (:file "global/date")
               (:file "global/time")
               (:file "entity/item")
               (:file "entity/transaction")
               (:file "use-case/transaction-with-items")
               (:file "repository/tree/package")
               (:file "repository/tree/tree")
               (:file "repository/tree/transaction")
               (:file "repository/tree/item")
               (:file "repository/tree/transaction-with-items")))

(asdf:defsystem #:kakeibo/test
  :description "Tests for kakeibo"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:kakeibo
               #:coalton/testing)
  :perform (asdf:test-op (o s)
                         (unless (symbol-call
                                  :fiasco
                                  :run-package-tests
                                  :package
                                  '#:kakeibo-test-fiasco)
                           (error "Tests failed")))
  :serial t
  :pathname "test/"
  :components ((:file "fiasco")
               (:file "global/date")
               (:file "entity/item")
               (:file "entity/transaction")
               (:file "entity/repository")
               (:file "use-case/repository")
               (:file "repository/tree")))
