(asdf:defsystem #:kakeibo
  :description "Managing my personal household accounts"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "class/valid")
               (:file "entity/date")))

(asdf:defsystem #:kakeibo/test
  :description "Tests for kakeibo"
  :author "Masaya Tojo"
  :license  "BSD"
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
               (:file "entity/date")))
